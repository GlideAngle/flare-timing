import System.Environment (getProgName)
import Text.Printf (printf)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Data.List ((\\), nub, sort, find)
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
import  Network.Wai.Middleware.Gzip (gzip, def)
import System.IO (hPutStrLn, stderr)
import Control.Exception.Safe (MonadThrow, catchIO)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader, asks, runReaderT)
import Control.Monad.Except (ExceptT(..), MonadError)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)

import System.FilePath (takeFileName)

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
    , IxTask(..)
    , PilotId(..)
    , Pilot(..)
    , PilotTaskStatus(..)
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
    ( OptimalRoute(..), TaskTrack(..)
    , TrackLine(..), ProjectedTrackLine(..), PlanarTrackLine(..)
    )
import Flight.Mask (checkTracks)
import ServeOptions (description)
import ServeTrack (RawLatLngTrack(..))
import Data.Ratio.Rounding (dpRound)

data Config k
    = Config
        { compFile :: CompInputFile
        , compSettings :: CompSettings k
        , routing :: Maybe [Maybe TaskTrack]
        , crossing :: Maybe Cg.Crossing
        , pointing :: Maybe Pointing
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
        , MonadThrow
        )

type CompInputApi k =
    "comp-input" :> "comps"
        :> Get '[JSON] Comp
    :<|> "comp-input" :> "nominals"
        :> Get '[JSON] Nominal
    :<|> "comp-input" :> "tasks"
        :> Get '[JSON] [Task k]
    :<|> "comp-input" :> "pilots"
        :> Get '[JSON] [Pilot]

type TaskLengthApi k =
    "comp-input" :> "comps"
        :> Get '[JSON] Comp
    :<|> "comp-input" :> "nominals"
        :> Get '[JSON] Nominal
    :<|> "comp-input" :> "tasks"
        :> Get '[JSON] [Task k]
    :<|> "comp-input" :> "pilots"
        :> Get '[JSON] [Pilot]

    :<|> "task-length" :> Capture "task" Int :> "spherical-edge"
        :> Get '[JSON] (OptimalRoute (Maybe TrackLine))

    :<|> "task-length" :> Capture "task" Int :> "ellipsoid-edge"
        :> Get '[JSON] (OptimalRoute (Maybe TrackLine))

    :<|> "task-length" :> Capture "task" Int :> "projected-edge-spherical"
        :> Get '[JSON] TrackLine

    :<|> "task-length" :> Capture "task" Int :> "projected-edge-ellipsoid"
        :> Get '[JSON] TrackLine

    :<|> "task-length" :> Capture "task" Int :> "projected-edge-planar"
        :> Get '[JSON] PlanarTrackLine

type GapPointApi k =
    "comp-input" :> "comps"
        :> Get '[JSON] Comp
    :<|> "comp-input" :> "nominals"
        :> Get '[JSON] Nominal
    :<|> "comp-input" :> "tasks"
        :> Get '[JSON] [Task k]
    :<|> "comp-input" :> "pilots"
        :> Get '[JSON] [Pilot]

    :<|> "task-length" :> Capture "task" Int :> "spherical-edge"
        :> Get '[JSON] (OptimalRoute (Maybe TrackLine))

    :<|> "task-length" :> Capture "task" Int :> "ellipsoid-edge"
        :> Get '[JSON] (OptimalRoute (Maybe TrackLine))

    :<|> "task-length" :> Capture "task" Int :> "projected-edge-spherical"
        :> Get '[JSON] TrackLine

    :<|> "task-length" :> Capture "task" Int :> "projected-edge-ellipsoid"
        :> Get '[JSON] TrackLine

    :<|> "task-length" :> Capture "task" Int :> "projected-edge-planar"
        :> Get '[JSON] PlanarTrackLine

    :<|> "gap-point" :> "pilots-status"
        :> Get '[JSON] [(Pilot, [PilotTaskStatus])]
    :<|> "gap-point" :> "validity"
        :> Get '[JSON] [Maybe Vy.Validity]
    :<|> "gap-point" :> "allocation"
        :> Get '[JSON] [Maybe Allocation]
    :<|> "gap-point" :> Capture "task" Int :> "score"
        :> Get '[JSON] [(Pilot, Breakdown)]
    :<|> "gap-point" :> Capture "task" Int :> "validity-working"
        :> Get '[JSON] (Maybe Vy.ValidityWorking)
    :<|> "cross-zone" :> Capture "task" Int :> "pilot-dnf"
        :> Get '[JSON] [Pilot]
    :<|> "cross-zone" :> Capture "task" Int :> "pilot-nyp"
        :> Get '[JSON] [Pilot]
    :<|> "gap-point" :> Capture "task" Int :> "pilot-df"
        :> Get '[JSON] [Pilot]

    :<|> "pilot-track" :> (Capture "task" Int) :> (Capture "pilot" String)
        :> Get '[JSON] RawLatLngTrack

compInputApi :: Proxy (CompInputApi k)
compInputApi = Proxy

taskLengthApi :: Proxy (TaskLengthApi k)
taskLengthApi = Proxy

gapPointApi :: Proxy (GapPointApi k)
gapPointApi = Proxy

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

    compSettings <-
        catchIO
            (Just <$> readComp compFile)
            (const $ return Nothing)

    crossing <-
        catchIO
            (Just <$> readCrossing crossFile)
            (const $ return Nothing)

    routes <-
        catchIO
            (Just <$> readRoute lenFile)
            (const $ return Nothing)

    pointing <-
        catchIO
            (Just <$> readPointing pointFile)
            (const $ return Nothing)

    case (compSettings, routes, crossing, pointing) of
        (Nothing, _, _, _) -> putStrLn "Couldn't read the comp settings"
        (Just cs, Nothing, _, _) ->
            f =<< mkCompInputApp (Config compFile cs Nothing Nothing Nothing)
        (Just cs, Just rt, Nothing, _) ->
            f =<< mkTaskLengthApp (Config compFile cs (Just rt) Nothing Nothing)
        (Just cs, Just rt, _, Nothing) ->
            f =<< mkTaskLengthApp (Config compFile cs (Just rt) Nothing Nothing)
        (Just cs, Just rt, Just cz, Just gp) -> do
            f =<< mkGapPointApp (Config compFile cs (Just rt) (Just cz) (Just gp))
    where
        -- NOTE: Add gzip with wai gzip middleware.
        -- SEE: https://github.com/haskell-servant/servant/issues/786
        f = runSettings settings . gzip def

        port = 3000

        settings =
            setPort port $
            setBeforeMainLoop
                (hPutStrLn stderr ("listening on port " ++ show port))
                defaultSettings

-- SEE: https://stackoverflow.com/questions/42143155/acess-a-servant-server-with-a-reflex-dom-client
mkCompInputApp :: Config k -> IO Application
mkCompInputApp cfg = return . simpleCors . serve compInputApi $ serverCompInputApi cfg

mkTaskLengthApp :: Config k -> IO Application
mkTaskLengthApp cfg = return . simpleCors . serve taskLengthApi $ serverTaskLengthApi cfg

mkGapPointApp :: Config k -> IO Application
mkGapPointApp cfg = return . simpleCors . serve gapPointApi $ serverGapPointApi cfg

serverCompInputApi :: Config k -> Server (CompInputApi k)
serverCompInputApi cfg =
    hoistServer compInputApi (convertApp cfg) $
        comp <$> c
        :<|> nominal <$> c
        :<|> tasks <$> c
        :<|> getPilots <$> c
    where
        c = asks compSettings

serverTaskLengthApi :: Config k -> Server (TaskLengthApi k)
serverTaskLengthApi cfg =
    hoistServer taskLengthApi (convertApp cfg) $
        comp <$> c
        :<|> nominal <$> c
        :<|> tasks <$> c
        :<|> getPilots <$> c
        :<|> getTaskRouteSphericalEdge
        :<|> getTaskRouteEllipsoidEdge
        :<|> getTaskRouteProjectedSphericalEdge
        :<|> getTaskRouteProjectedEllipsoidEdge
        :<|> getTaskRouteProjectedPlanarEdge
    where
        c = asks compSettings

serverGapPointApi :: Config k -> Server (GapPointApi k)
serverGapPointApi cfg =
    hoistServer gapPointApi (convertApp cfg) $
        comp <$> c
        :<|> nominal <$> c
        :<|> tasks <$> c
        :<|> getPilots <$> c
        :<|> getTaskRouteSphericalEdge
        :<|> getTaskRouteEllipsoidEdge
        :<|> getTaskRouteProjectedSphericalEdge
        :<|> getTaskRouteProjectedEllipsoidEdge
        :<|> getTaskRouteProjectedPlanarEdge
        :<|> getPilotsStatus
        :<|> getValidity <$> p
        :<|> getAllocation <$> p
        :<|> getTaskScore
        :<|> getTaskValidityWorking
        :<|> getTaskPilotDnf
        :<|> getTaskPilotNyp
        :<|> getTaskPilotDf
        :<|> getTaskPilotTrack
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

getValidity :: Maybe Pointing -> [Maybe Vy.Validity]
getValidity Nothing = []
getValidity (Just p) = ((fmap . fmap) roundValidity) . validity $ p

getAllocation :: Maybe Pointing -> [Maybe Allocation]
getAllocation Nothing = []
getAllocation (Just p) = ((fmap . fmap) roundAllocation) . allocation $ p

getScores :: Pointing -> [[(Pilot, Breakdown)]]
getScores = ((fmap . fmap . fmap) roundVelocity') . score

getTaskScore :: Int -> AppT k IO [(Pilot, Breakdown)]
getTaskScore ii = do
    xs' <- fmap getScores <$> asks pointing
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "gap-point" ii

getTaskValidityWorking :: Int -> AppT k IO (Maybe Vy.ValidityWorking)
getTaskValidityWorking ii = do
    xs' <- fmap validityWorking <$> asks pointing
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "gap-point" ii

getTaskRouteSphericalEdge :: Int -> AppT k IO (OptimalRoute (Maybe TrackLine))
getTaskRouteSphericalEdge ii = do
    xs' <- asks routing
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                Just TaskTrack{sphericalEdgeToEdge = x} : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "task-length" ii

getTaskRouteEllipsoidEdge :: Int -> AppT k IO (OptimalRoute (Maybe TrackLine))
getTaskRouteEllipsoidEdge ii = do
    xs' <- asks routing
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                Just TaskTrack{ellipsoidEdgeToEdge = x} : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "task-length" ii

getTaskRouteProjectedSphericalEdge :: Int -> AppT k IO TrackLine
getTaskRouteProjectedSphericalEdge ii = do
    xs' <- asks routing
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                Just
                   TaskTrack
                       { projection =
                           OptimalRoute
                               { taskRoute =
                                   Just
                                       ProjectedTrackLine
                                           { spherical = x }}} : _ -> return x

                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "task-length" ii

getTaskRouteProjectedEllipsoidEdge :: Int -> AppT k IO TrackLine
getTaskRouteProjectedEllipsoidEdge ii = do
    xs' <- asks routing
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                Just
                   TaskTrack
                       { projection =
                           OptimalRoute
                               { taskRoute =
                                   Just
                                       ProjectedTrackLine
                                           { ellipsoid = x }}} : _ -> return x

                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "task-length" ii

getTaskRouteProjectedPlanarEdge :: Int -> AppT k IO PlanarTrackLine
getTaskRouteProjectedPlanarEdge ii = do
    xs' <- asks routing
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                Just
                   TaskTrack
                       { projection =
                           OptimalRoute
                               { taskRoute =
                                   Just
                                       ProjectedTrackLine
                                           { planar = x }}} : _ -> return x

                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "task-length" ii

getTaskPilotDnf :: Int -> AppT k IO [Pilot]
getTaskPilotDnf ii = do
    xss' <- fmap (\Cg.Crossing{dnf} -> dnf) <$> asks crossing
    case xss' of
        Just xss ->
            case drop (ii - 1) xss of
                xs : _ -> return xs
                _ -> throwError $ errTaskBounds ii

        _ -> return []

nyp
    :: [Pilot]
    -> Task a
    -> [Pilot]
    -> [(Pilot, b)]
    -> [Pilot]
nyp ps Task{absent = ys} xs cs =
    let (cs', _) = unzip cs in ps \\ (xs ++ ys ++ cs')

status
    :: Task a -- ^ The tasks for which we're getting the status
    -> [Pilot] -- ^ Pilots that DNF this task
    -> [Pilot] -- ^ Pilots that DF this task
    -> Pilot -- ^ Get the status for this pilot
    -> PilotTaskStatus
status Task{absent = ys} xs cs p =
    if | p `elem` ys -> ABS
       | p `elem` xs -> DNF
       | p `elem` cs -> DF
       | otherwise -> NYP

getTaskPilotNyp :: Int -> AppT k IO [Pilot]
getTaskPilotNyp ii = do
    let jj = ii - 1
    ps <- getPilots <$> asks compSettings
    ts <- tasks <$> asks compSettings
    xss' <- fmap (\Cg.Crossing{dnf} -> dnf) <$> asks crossing
    css' <- fmap getScores <$> asks pointing
    case (xss', css') of
        (Just xss, Just css) ->
            case (drop jj ts, drop jj xss, drop jj css) of
                (t : _, xs : _, cs : _) -> return $ nyp ps t xs cs
                _ -> throwError $ errTaskBounds ii

        _ -> return ps

getTaskPilotDf :: Int -> AppT k IO [Pilot]
getTaskPilotDf ii = do
    let jj = ii - 1
    ps <- getPilots <$> asks compSettings
    css' <- fmap getScores <$> asks pointing
    case css' of
        Just css ->
            case drop jj css of
                cs : _ -> return . fst . unzip $ cs
                _ -> throwError $ errTaskBounds ii

        _ -> return ps

getPilotsStatus :: AppT k IO [(Pilot, [PilotTaskStatus])]
getPilotsStatus = do
    ps <- getPilots <$> asks compSettings
    ts <- tasks <$> asks compSettings
    xss' <- fmap (\Cg.Crossing{dnf} -> dnf) <$> asks crossing
    css' <- fmap getScores <$> asks pointing

    let fs =
            case (xss', css') of
              (Just xss, Just css) ->
                    [ status t xs $ fst <$> cs
                    | t <- ts
                    | xs <- xss
                    | cs <- css
                    ]

              _ -> repeat $ const NYP

    return $ [(p,) $ ($ p) <$> fs | p <- ps]

getTaskPilotTrack :: Int -> String -> AppT k IO RawLatLngTrack
getTaskPilotTrack ii pilotId = do
    let jj = ii - 1
    let ix = IxTask ii
    let pilot = PilotId pilotId
    cf <- asks compFile
    ps <- getPilots <$> asks compSettings
    let p = find (\(Pilot (pid, _)) -> pid == pilot) ps

    case p of
        Nothing -> throwError $ errPilotNotFound pilot
        Just p' -> do
            t <- checkTracks (const $ const id) cf [ix] [p']
            case take 1 $ drop jj t of
                [t' : _] ->
                    case t' of
                        Right (_, mf) -> return $ RawLatLngTrack mf
                        _ -> throwError $ errPilotTrackNotFound ix pilot
                _ -> throwError $ errPilotTrackNotFound ix pilot

errPilotTrackNotFound :: IxTask -> PilotId -> ServantErr
errPilotTrackNotFound (IxTask ix) (PilotId p) =
    err400
        { errBody = LBS.pack
        $ printf "For task %d, the tracklog for pilot %s was not found" ix p
        }

errPilotNotFound :: PilotId -> ServantErr
errPilotNotFound (PilotId p) =
    err400 {errBody = LBS.pack $ printf "Pilot %s not found" p}

errTaskBounds :: Int -> ServantErr
errTaskBounds ii =
    err400 {errBody = LBS.pack $ printf "Out of bounds task %d" ii}

errTaskStep :: String -> Int -> ServantErr
errTaskStep step ii =
    err400
        { errBody = LBS.pack
        $ "I need to have access to data from "
        ++ step
        ++ " for task: #"
        ++ show ii
        }
