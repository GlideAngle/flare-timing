import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Data.List (nub, sort)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, setPort, setBeforeMainLoop)
import Servant
    ( (:<|>)(..)
    , Get, JSON, Server, Handler(..), Proxy(..), ServantErr
    , (:>)
    , hoistServer, serve
    )
import System.IO (hPutStrLn, stderr)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader, asks, runReaderT)
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT)

import System.FilePath (takeFileName)
import Data.Yaml (prettyPrintParseException)

import Flight.Track.Cross (Crossing(..))
import Flight.Track.Point
    (Pointing(..), Velocity(..), Allocation(..), Breakdown(..))
import qualified Flight.Score as Wg (Weights(..))
import qualified Flight.Score as Vy (Validity(..))
import Flight.Score
    ( PilotVelocity(..)
    , DistanceWeight(..), LeadingWeight(..), ArrivalWeight(..), TimeWeight(..)
    , DistanceValidity(..), LaunchValidity(..), TaskValidity(..), TimeValidity(..)
    )
import Flight.Scribe (readComp, readCrossing, readPointing)
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.ServeOptions (CmdServeOptions(..), mkOptions)
import Flight.Comp
    ( FileType(CompInput)
    , CompSettings(..)
    , Comp
    , Task
    , Nominal(..)
    , PilotTrackLogFile(..)
    , Pilot(..)
    , CompInputFile(..)
    , CrossZoneFile(..)
    , GapPointFile(..)
    , findCompInput
    , compToCross
    , compToPoint
    , ensureExt
    )
import ServeOptions (description)
import Data.Ratio.Rounding (dpRound)

data Config k
    = Config
        { compSettings :: CompSettings k
        , crossing :: Crossing
        , pointing :: Pointing
        }

newtype AppT k m a =
    AppT
        { unApp :: ReaderT (Config k) (ExceptT ServantErr m) a
        }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader (Config k)
        , MonadError ServantErr
        , MonadIO
        )

type Api k =
    "comps" :> Get '[JSON] Comp
    :<|> "nominals" :> Get '[JSON] Nominal
    :<|> "tasks" :> Get '[JSON] [Task k]
    :<|> "pilots" :> Get '[JSON] [Pilot]
    :<|> "gap-point" :> "validity" :> Get '[JSON] [Maybe Vy.Validity]
    :<|> "gap-point" :> "allocation" :> Get '[JSON] [Maybe Allocation]
    :<|> "gap-point" :> "score" :> Get '[JSON] [[(Pilot, Breakdown)]]

api :: Proxy (Api k)
api = Proxy

convertApp :: Config k -> AppT k IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (unApp appt) cfg

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = ensureExt CompInput}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdServeOptions -> IO ()
drive o = do
    files <- findCompInput o
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files
go :: CmdServeOptions -> CompInputFile -> IO ()
go CmdServeOptions{..} compFile@(CompInputFile compPath) = do
    let crossFile@(CrossZoneFile crossPath) = compToCross compFile
    let pointFile@(GapPointFile pointPath) = compToPoint compFile
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading pilots that did not fly from '" ++ takeFileName crossPath ++ "'"
    putStrLn $ "Reading scores from '" ++ takeFileName pointPath ++ "'"

    compSettings <- runExceptT $ readComp compFile
    crossing <- runExceptT $ readCrossing crossFile
    pointing <- runExceptT $ readPointing pointFile

    let ppr = putStrLn . prettyPrintParseException

    case (compSettings, crossing, pointing) of
        (Left e, _, _) -> ppr e
        (_, Left e, _) -> ppr e
        (_, _, Left e) -> ppr e
        (Right cs, Right cz, Right gp) ->
            runSettings settings =<< mkApp (Config cs cz gp)
    where
        port = 3000

        settings =
            setPort port $
            setBeforeMainLoop
                (hPutStrLn stderr ("listening on port " ++ show port))
                defaultSettings

-- SEE: https://stackoverflow.com/questions/42143155/acess-a-servant-server-with-a-reflex-dom-client
mkApp :: Config k -> IO Application
mkApp cfg = return . simpleCors . serve api $ serverApi cfg

serverApi :: Config k -> Server (Api k)
serverApi cfg =
    hoistServer api (convertApp cfg) $
        comp <$> asks compSettings
        :<|> nominal <$> asks compSettings
        :<|> tasks <$> asks compSettings
        :<|> getPilots <$> asks compSettings
        :<|> getValidity <$> asks pointing
        :<|> getAllocation <$> asks pointing
        :<|> getScore <$> asks pointing

distinctPilots :: [[PilotTrackLogFile]] -> [Pilot]
distinctPilots pss =
    let pilot (PilotTrackLogFile p _) = p
    in sort . nub .concat $ (fmap . fmap) pilot pss

roundVelocity
    :: PilotVelocity (Quantity Double [u| km / h |])
    -> PilotVelocity (Quantity Double [u| km / h |])
roundVelocity (PilotVelocity (MkQuantity d)) =
    PilotVelocity . MkQuantity . fromRational . (dpRound 1) . toRational $ d

roundVelocity' :: Breakdown -> Breakdown
roundVelocity' b@Breakdown{velocity = v@Velocity{gsVelocity = (Just x)}} =
    b{velocity = v{gsVelocity = Just . roundVelocity $ x}}
roundVelocity' b = b

roundWeights :: Wg.Weights -> Wg.Weights
roundWeights
    Wg.Weights
        { distance = DistanceWeight dw
        , leading = LeadingWeight lw
        , arrival = ArrivalWeight aw
        , time = TimeWeight tw
        } =
    Wg.Weights
        { distance = DistanceWeight $ dpRound 3 dw
        , leading = LeadingWeight $ dpRound 3 lw
        , arrival = ArrivalWeight $ dpRound 3 aw
        , time = TimeWeight $ dpRound 3 tw
        }

roundValidity :: Vy.Validity -> Vy.Validity
roundValidity
    Vy.Validity
        { launch = LaunchValidity ly
        , distance = DistanceValidity dy
        , time = TimeValidity ty
        , task = TaskValidity ky
        } =
    Vy.Validity
        { launch = LaunchValidity $ dpRound 3 ly
        , distance = DistanceValidity $ dpRound 3 dy
        , time = TimeValidity $ dpRound 3 ty
        , task = TaskValidity $ dpRound 3 ky
        }

roundAllocation:: Allocation -> Allocation
roundAllocation x@Allocation{..} =
    x{ weight = roundWeights weight }

getPilots :: CompSettings k -> [Pilot]
getPilots = distinctPilots . pilots

getValidity :: Pointing -> [Maybe Vy.Validity]
getValidity = ((fmap . fmap) roundValidity) . validity

getAllocation :: Pointing -> [Maybe Allocation]
getAllocation = ((fmap . fmap) roundAllocation) . allocation

getScore :: Pointing -> [[(Pilot, Breakdown)]]
getScore = ((fmap . fmap . fmap) roundVelocity') . score
