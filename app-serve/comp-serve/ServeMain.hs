import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Data.List ((\\), nub, sort)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, setPort, setBeforeMainLoop)
import Servant
    ( (:<|>)(..)
    , Capture, Get, JSON, Server, Handler(..), Proxy(..), ServantErr
    , (:>)
    , errBody, err400, hoistServer, serve, throwError
    )
import System.IO (hPutStrLn, stderr)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader, asks, runReaderT)
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)

import System.FilePath (takeFileName)
import Data.Yaml (prettyPrintParseException)

import qualified Flight.Track.Cross as Cg (Crossing(..))
import Flight.Track.Point
    (Pointing(..), Velocity(..), Allocation(..), Breakdown(..))
import qualified Flight.Score as Wg (Weights(..))
import qualified Flight.Score as Vy (Validity(..), ValidityWorking(..))
import Flight.Score
    ( PilotVelocity(..)
    , DistanceWeight(..), LeadingWeight(..), ArrivalWeight(..), TimeWeight(..)
    , DistanceValidity(..), LaunchValidity(..), TaskValidity(..), TimeValidity(..)
    )
import Flight.Scribe (readComp, readRoute, readCrossing, readPointing)
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.ServeOptions (CmdServeOptions(..), mkOptions)
import Flight.Comp
    ( FileType(CompInput)
    , CompSettings(..)
    , Comp
    , Task(..)
    , Nominal(..)
    , PilotTrackLogFile(..)
    , Pilot(..)
    , CompInputFile(..)
    , TaskLengthFile(..)
    , CrossZoneFile(..)
    , GapPointFile(..)
    , findCompInput
    , compToTaskLength
    , compToCross
    , compToPoint
    , ensureExt
    )
import Flight.Route
import ServeOptions (description)
import Data.Ratio.Rounding (dpRound)

data Config k
    = Config
        { compSettings :: CompSettings k
        , crossing :: Cg.Crossing
        , routing :: [Maybe TaskTrack]
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
    "comps"
        :> Get '[JSON] Comp
    :<|> "nominals"
        :> Get '[JSON] Nominal
    :<|> "tasks"
        :> Get '[JSON] [Task k]
    :<|> "pilots"
        :> Get '[JSON] [Pilot]
    :<|> "gap-point" :> "validity"
        :> Get '[JSON] [Maybe Vy.Validity]
    :<|> "gap-point" :> "allocation"
        :> Get '[JSON] [Maybe Allocation]
    :<|> "gap-point" :> Capture "task" Int :> "score"
        :> Get '[JSON] [(Pilot, Breakdown)]
    :<|> "gap-point" :> Capture "task" Int :> "validity-working"
        :> Get '[JSON] (Maybe Vy.ValidityWorking)
    :<|> "task-length" :> Capture "task" Int :> "spherical-edge"
        :> Get '[JSON] (OptimalRoute (Maybe TrackLine))
    :<|> "cross-zone" :> Capture "task" Int :> "pilot-dnf"
        :> Get '[JSON] [Pilot]
    :<|> "cross-zone" :> Capture "task" Int :> "pilot-nyp"
        :> Get '[JSON] [Pilot]

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
    let lenFile@(TaskLengthFile lenPath) = compToTaskLength compFile
    let crossFile@(CrossZoneFile crossPath) = compToCross compFile
    let pointFile@(GapPointFile pointPath) = compToPoint compFile
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading task length from '" ++ takeFileName lenPath ++ "'"
    putStrLn $ "Reading pilots that did not fly from '" ++ takeFileName crossPath ++ "'"
    putStrLn $ "Reading scores from '" ++ takeFileName pointPath ++ "'"

    compSettings <- runExceptT $ readComp compFile
    crossing <- runExceptT $ readCrossing crossFile
    routes <- runExceptT $ readRoute lenFile
    pointing <- runExceptT $ readPointing pointFile

    let ppr = putStrLn . prettyPrintParseException

    case (compSettings, crossing, routes, pointing) of
        (Left e, _, _, _) -> ppr e
        (_, Left e, _, _) -> ppr e
        (_, _, Left e, _) -> ppr e
        (_, _, _, Left e) -> ppr e
        (Right cs, Right cz, Right rt, Right gp) ->
            runSettings settings =<< mkApp (Config cs cz rt gp)
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
        comp <$> c
        :<|> nominal <$> c
        :<|> tasks <$> c
        :<|> getPilots <$> c
        :<|> getValidity <$> p
        :<|> getAllocation <$> p
        :<|> getTaskScore
        :<|> getTaskValidityWorking
        :<|> getTaskRouteSphericalEdge
        :<|> getTaskPilotDnf
        :<|> getTaskPilotNyp
    where
        c = asks compSettings
        p = asks pointing

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

dpWg :: Rational -> Rational
dpWg = dpRound 4

dpVy :: Rational -> Rational
dpVy = dpRound 3

roundWeights :: Wg.Weights -> Wg.Weights
roundWeights
    Wg.Weights
        { distance = DistanceWeight dw
        , leading = LeadingWeight lw
        , arrival = ArrivalWeight aw
        , time = TimeWeight tw
        } =
    Wg.Weights
        { distance = DistanceWeight $ dpWg dw
        , leading = LeadingWeight $ dpWg lw
        , arrival = ArrivalWeight $ dpWg aw
        , time = TimeWeight $ dpWg tw
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
        { launch = LaunchValidity $ dpVy ly
        , distance = DistanceValidity $ dpVy dy
        , time = TimeValidity $ dpVy ty
        , task = TaskValidity $ dpVy ky
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

getScores :: Pointing -> [[(Pilot, Breakdown)]]
getScores = ((fmap . fmap . fmap) roundVelocity') . score

getTaskScore :: Int -> AppT k IO [(Pilot, Breakdown)]
getTaskScore ii = do
    xs <- getScores <$> asks pointing
    case drop (ii - 1) xs of
        x : _ -> return x
        _ -> throwError $ errTaskBounds ii

getTaskValidityWorking :: Int -> AppT k IO (Maybe Vy.ValidityWorking)
getTaskValidityWorking ii = do
    xs <- validityWorking <$> asks pointing
    case drop (ii - 1) xs of
        x : _ -> return x
        _ -> throwError $ errTaskBounds ii

getTaskRouteSphericalEdge :: Int -> AppT k IO (OptimalRoute (Maybe TrackLine))
getTaskRouteSphericalEdge ii = do
    xs <- asks routing
    case drop (ii - 1) xs of
        Just TaskTrack{sphericalEdgeToEdge = x} : _ -> return x
        _ -> throwError $ errTaskBounds ii

getTaskPilotDnf :: Int -> AppT k IO [Pilot]
getTaskPilotDnf ii = do
    xss <- (\Cg.Crossing{dnf} -> dnf) <$> asks crossing
    case drop (ii - 1) xss of
        xs : _ -> return xs
        _ -> throwError $ errTaskBounds ii

getTaskPilotNyp :: Int -> AppT k IO [Pilot]
getTaskPilotNyp ii = do
    ps <- getPilots <$> asks compSettings
    ts <- tasks <$> asks compSettings
    xss <- (\Cg.Crossing{dnf} -> dnf) <$> asks crossing
    css <- getScores <$> asks pointing
    let jj = ii - 1
    case (drop jj ts, drop jj xss, drop jj css) of
        ( Task{absent = ys} : _, xs : _, cs : _) ->
            let (cs', _) = unzip cs in return $ ps \\ (xs ++ ys ++ cs')

        _ -> throwError $ errTaskBounds ii

errTaskBounds :: Int -> ServantErr
errTaskBounds ii =
    err400
        { errBody = LBS.pack
        $ "Out of bounds task: #" ++ show ii
        }
