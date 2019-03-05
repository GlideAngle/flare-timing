import Prelude hiding (abs)
import System.Environment (getProgName)
import Text.Printf (printf)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Data.Maybe (isNothing, catMaybes)
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
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader, asks, runReaderT)
import Control.Monad.Except (ExceptT(..), MonadError)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)

import System.FilePath (takeFileName)

import qualified Flight.Track.Cross as Cg (Crossing(..))
import Flight.Track.Cross (TrackFlyingSection(..))
import Flight.Track.Distance (TrackReach(..))
import Flight.Track.Land (Landing(..), TrackEffort(..), effortRank)
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Lead (TrackLead(..))
import Flight.Track.Speed (TrackSpeed(..))
import Flight.Track.Mask (Masking(..))
import qualified Flight.Track.Point as Norm (NormPointing(..), NormBreakdown(..))
import Flight.Track.Point
    (Pointing(..), Velocity(..), Allocation(..), Breakdown(..))
import qualified Flight.Score as Wg (Weights(..))
import qualified Flight.Score as Vy (Validity(..), ValidityWorking(..))
import Flight.Score
    ( PilotVelocity(..)
    , DistanceWeight(..), ReachWeight(..), EffortWeight(..)
    , LeadingWeight(..), ArrivalWeight(..), TimeWeight(..)
    , DistanceValidity(..), LaunchValidity(..), TaskValidity(..), TimeValidity(..)
    )
import Flight.Scribe
    ( readComp, readScore
    , readRoute, readCrossing, readMasking, readLanding, readPointing
    )
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.ServeOptions (CmdServeOptions(..), mkOptions)
import Flight.Comp
    ( FileType(CompInput)
    , CompSettings(..)
    , EarthModel(..)
    , EarthMath(..)
    , Comp(..)
    , Task(..)
    , Nominal(..)
    , PilotTrackLogFile(..)
    , IxTask(..)
    , PilotId(..)
    , Pilot(..)
    , PilotTaskStatus(..)
    , PilotGroup(..)
    , DfNoTrack(..)
    , DfNoTrackPilot(..)
    , Nyp(..)
    , CompInputFile(..)
    , TaskLengthFile(..)
    , CrossZoneFile(..)
    , MaskTrackFile(..)
    , LandOutFile(..)
    , GapPointFile(..)
    , NormScoreFile(..)
    , findCompInput
    , compToScore
    , compToTaskLength
    , compToCross
    , compToMask
    , compToLand
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
import Flight.Distance (QTaskDistance)

data Config k
    = Config
        { compFile :: CompInputFile
        , compSettings :: CompSettings k
        , routing :: Maybe [Maybe TaskTrack]
        , crossing :: Maybe Cg.Crossing
        , masking :: Maybe Masking
        , landing :: Maybe Landing
        , pointing :: Maybe Pointing
        , norming :: Maybe Norm.NormPointing
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

    :<|> "task-length" :> "task-lengths"
        :> Get '[JSON] [QTaskDistance Double [u| m |]]

type GapPointApi k =
    "comp-input" :> "comps"
        :> Get '[JSON] Comp
    :<|> "comp-input" :> "nominals"
        :> Get '[JSON] Nominal
    :<|> "comp-input" :> "tasks"
        :> Get '[JSON] [Task k]
    :<|> "comp-input" :> "pilots"
        :> Get '[JSON] [Pilot]

    :<|> "fs-score" :> (Capture "task" Int) :> "score"
        :> Get '[JSON] [(Pilot, Norm.NormBreakdown)]

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

    :<|> "task-length" :> "task-lengths"
        :> Get '[JSON] [QTaskDistance Double [u| m |]]

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

    :<|> "comp-input" :> Capture "task" Int :> "pilot-abs"
        :> Get '[JSON] [Pilot]
    :<|> "comp-input" :> Capture "task" Int :> "pilot-dnf"
        :> Get '[JSON] [Pilot]
    :<|> "comp-input" :> Capture "task" Int :> "pilot-dfnt"
        :> Get '[JSON] [DfNoTrackPilot]
    :<|> "gap-point" :> Capture "task" Int :> "pilot-nyp"
        :> Get '[JSON] [Pilot]
    :<|> "gap-point" :> Capture "task" Int :> "pilot-df"
        :> Get '[JSON] [Pilot]

    :<|> "pilot-track" :> (Capture "task" Int) :> (Capture "pilot" String)
        :> Get '[JSON] RawLatLngTrack

    :<|> "cross-zone" :> "track-flying-section" :> (Capture "task" Int) :> (Capture "pilot" String)
        :> Get '[JSON] TrackFlyingSection

    :<|> "mask-track" :> (Capture "task" Int) :> "reach"
        :> Get '[JSON] [(Pilot, TrackReach)]

    :<|> "mask-track" :> (Capture "task" Int) :> "arrival"
        :> Get '[JSON] [(Pilot, TrackArrival)]

    :<|> "mask-track" :> (Capture "task" Int) :> "lead"
        :> Get '[JSON] [(Pilot, TrackLead)]

    :<|> "mask-track" :> (Capture "task" Int) :> "time"
        :> Get '[JSON] [(Pilot, TrackSpeed)]

    :<|> "land-out" :> (Capture "task" Int) :> "effort"
        :> Get '[JSON] [(Pilot, TrackEffort)]

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
    let maskFile@(MaskTrackFile maskPath) = compToMask compFile
    let landFile@(LandOutFile landPath) = compToLand compFile
    let pointFile@(GapPointFile pointPath) = compToPoint compFile
    let normFile@(NormScoreFile normPath) = compToScore compFile
    putStrLn $ "Reading competition & pilots DNF from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading flying time range from '" ++ takeFileName crossPath ++ "'"
    putStrLn $ "Reading task length from '" ++ takeFileName lenPath ++ "'"
    putStrLn $ "Reading arrivals from '" ++ takeFileName maskPath ++ "'"
    putStrLn $ "Reading land outs from '" ++ takeFileName landPath ++ "'"
    putStrLn $ "Reading scores from '" ++ takeFileName pointPath ++ "'"
    putStrLn $ "Reading expected or normative scores from '" ++ takeFileName normPath ++ "'"

    compSettings <-
        catchIO
            (Just <$> readComp compFile)
            (const $ return Nothing)

    routes <-
        catchIO
            (Just <$> readRoute lenFile)
            (const $ return Nothing)

    crossing <-
        catchIO
            (Just <$> readCrossing crossFile)
            (const $ return Nothing)

    masking <-
        catchIO
            (Just <$> readMasking maskFile)
            (const $ return Nothing)

    landing <-
        catchIO
            (Just <$> readLanding landFile)
            (const $ return Nothing)

    pointing <-
        catchIO
            (Just <$> readPointing pointFile)
            (const $ return Nothing)

    norming <-
        catchIO
            (Just <$> readScore normFile)
            (const $ return Nothing)

    case (compSettings, routes, crossing, masking, landing, pointing, norming) of
        (Nothing, _, _, _, _, _, _) -> putStrLn "Couldn't read the comp settings"
        (Just cs, rt@(Just _), cg@(Just _), mg@(Just _), lo@(Just _), gp@(Just _), ns@(Just _)) ->
            f =<< mkGapPointApp (Config compFile cs rt cg mg lo gp ns)
        (Just cs, _, _, _, _, _, _) ->
            f =<< mkCompInputApp (Config compFile cs Nothing Nothing Nothing Nothing Nothing Nothing)
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
        :<|> getTaskRouteLengths
    where
        c = asks compSettings

serverGapPointApi :: Config k -> Server (GapPointApi k)
serverGapPointApi cfg =
    hoistServer gapPointApi (convertApp cfg) $
        comp <$> c
        :<|> nominal <$> c
        :<|> tasks <$> c
        :<|> getPilots <$> c
        :<|> getTaskNormScore
        :<|> getTaskRouteSphericalEdge
        :<|> getTaskRouteEllipsoidEdge
        :<|> getTaskRouteProjectedSphericalEdge
        :<|> getTaskRouteProjectedEllipsoidEdge
        :<|> getTaskRouteProjectedPlanarEdge
        :<|> getTaskRouteLengths
        :<|> getPilotsStatus
        :<|> getValidity <$> p
        :<|> getAllocation <$> p
        :<|> getTaskScore
        :<|> getTaskValidityWorking
        :<|> getTaskPilotAbs
        :<|> getTaskPilotDnf
        :<|> getTaskPilotDfNoTrack
        :<|> getTaskPilotNyp
        :<|> getTaskPilotDf
        :<|> getTaskPilotTrack
        :<|> getTaskPilotTrackFlyingSection
        :<|> getTaskReach
        :<|> getTaskArrival
        :<|> getTaskLead
        :<|> getTaskTime
        :<|> getTaskEffort
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
roundVelocity' b@Breakdown{velocity = Just v@Velocity{gsVelocity = (Just x)}} =
    b{velocity = Just v{gsVelocity = Just . roundVelocity $ x}}
roundVelocity' b = b

dpWg :: Rational -> Rational
dpWg = dpRound 4

dpVy :: Rational -> Rational
dpVy = dpRound 3

roundWeights :: Wg.Weights -> Wg.Weights
roundWeights
    Wg.Weights
        { distance = DistanceWeight dw
        , reach = ReachWeight rw
        , effort = EffortWeight ew
        , leading = LeadingWeight lw
        , arrival = ArrivalWeight aw
        , time = TimeWeight tw
        } =
    Wg.Weights
        { distance = DistanceWeight $ dpWg dw
        , reach = ReachWeight $ dpWg rw
        , effort = EffortWeight $ dpWg ew
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

getScoresDf :: Pointing -> [[(Pilot, Breakdown)]]
getScoresDf = ((fmap . fmap . fmap) roundVelocity') . scoreDf

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

getTaskRouteLengths :: AppT k IO [QTaskDistance Double [u| m |]]
getTaskRouteLengths = do
    Comp{earth = e, earthMath = m} <- comp <$> asks compSettings
    xs' <- asks routing
    case xs' of
        Just xs -> do
            let ds = [join $ fmap (getRouteLength e m) x | x <- xs]

            if any isNothing ds
               then throwError errTaskLengths
               else return $ catMaybes ds

        _ -> throwError errTaskLengths

getRouteLength
    :: EarthModel
    -> EarthMath
    -> TaskTrack
    -> Maybe (QTaskDistance Double [u| m |])
getRouteLength (EarthAsSphere _) Haversines
    TaskTrack
        { sphericalEdgeToEdge =
            OptimalRoute
                { taskRoute =
                    Just
                        TrackLine
                            {distance = d}}} = Just d
getRouteLength (EarthAsSphere _) _ _ = Nothing
getRouteLength _ Haversines _ = Nothing
getRouteLength (EarthAsEllipsoid _) Vincenty
    TaskTrack
        { ellipsoidEdgeToEdge =
            OptimalRoute
                { taskRoute =
                    Just
                        TrackLine
                            {distance = d}}} = Just d
getRouteLength (EarthAsEllipsoid _) _ _ = Nothing
getRouteLength _ Vincenty _ = Nothing
getRouteLength _ Andoyer _ = Nothing -- TODO: Implement Andoyer algorithm for the ellipsoid
getRouteLength (EarthAsFlat _) Pythagorus
    TaskTrack
        { projection =
            OptimalRoute
                { taskRoute =
                    Just
                        ProjectedTrackLine
                            {spherical =
                                TrackLine
                                    {distance = d}}}} = Just d
getRouteLength (EarthAsFlat _) _ _ = Nothing

getTaskPilotAbs :: Int -> AppT k IO [Pilot]
getTaskPilotAbs ii = do
    pgs <- pilotGroups <$> asks compSettings
    case drop (ii - 1) pgs of
        PilotGroup{..} : _ -> return absent
        _ -> throwError $ errTaskBounds ii

getTaskPilotDnf :: Int -> AppT k IO [Pilot]
getTaskPilotDnf ii = do
    pgs <- pilotGroups <$> asks compSettings
    case drop (ii - 1) pgs of
        PilotGroup{..} : _ -> return dnf
        _ -> throwError $ errTaskBounds ii

getTaskPilotDfNoTrack :: Int -> AppT k IO [DfNoTrackPilot]
getTaskPilotDfNoTrack ii = do
    pgs <- pilotGroups <$> asks compSettings
    case drop (ii - 1) pgs of
        PilotGroup{didFlyNoTracklog = DfNoTrack zs} : _ -> return zs
        _ -> throwError $ errTaskBounds ii

nyp
    :: [Pilot]
    -> PilotGroup
    -> [(Pilot, b)]
    -> Nyp
nyp ps PilotGroup{absent = xs, dnf = ys, didFlyNoTracklog = DfNoTrack zs} cs =
    let (cs', _) = unzip cs in
    Nyp $ ps \\ (xs ++ ys ++ (pilot <$> zs) ++ cs')

status
    :: PilotGroup
    -> [Pilot] -- ^ Pilots that DF this task
    -> Pilot -- ^ Get the status for this pilot
    -> PilotTaskStatus
status PilotGroup{absent = xs, dnf = ys, didFlyNoTracklog = DfNoTrack zs} cs p =
    if | p `elem` xs -> ABS
       | p `elem` ys -> DNF
       | p `elem` cs -> DF
       | p `elem` (pilot <$> zs) -> DFNoTrack
       | otherwise -> NYP

getTaskPilotNyp :: Int -> AppT k IO [Pilot]
getTaskPilotNyp ii = do
    let jj = ii - 1
    ps <- getPilots <$> asks compSettings
    pgs <- pilotGroups <$> asks compSettings
    css' <- fmap getScoresDf <$> asks pointing
    case css' of
        Just css ->
            case (drop jj pgs, drop jj css) of
                (pg : _, cs : _) ->
                    let Nyp ps' = nyp ps pg cs in return ps'
                _ -> throwError $ errTaskBounds ii

        _ -> return ps

getTaskPilotDf :: Int -> AppT k IO [Pilot]
getTaskPilotDf ii = do
    let jj = ii - 1
    ps <- getPilots <$> asks compSettings
    css' <- fmap getScoresDf <$> asks pointing
    case css' of
        Just css ->
            case drop jj css of
                cs : _ -> return . fst . unzip $ cs
                _ -> throwError $ errTaskBounds ii

        _ -> return ps

getPilotsStatus :: AppT k IO [(Pilot, [PilotTaskStatus])]
getPilotsStatus = do
    ps <- getPilots <$> asks compSettings
    pgs <- pilotGroups <$> asks compSettings
    css' <- fmap getScoresDf <$> asks pointing

    let fs =
            case css' of
              Just css ->
                    [ status pg $ fst <$> cs
                    | pg <- pgs
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

getTaskPilotTrackFlyingSection :: Int -> String -> AppT k IO TrackFlyingSection
getTaskPilotTrackFlyingSection ii pilotId = do
    let jj = ii - 1
    let ix = IxTask ii
    let pilot = PilotId pilotId
    fss <- fmap Cg.flying <$> asks crossing
    let isPilot (Pilot (pid, _)) = pid == PilotId pilotId

    case fss of
        Nothing -> throwError $ errPilotNotFound pilot
        Just fss' -> do
            case take 1 $ drop jj fss' of
                fs : _ ->
                    case find (isPilot . fst) fs of
                        Just (_, Just y) -> return y
                        _ -> throwError $ errPilotTrackNotFound ix pilot
                _ -> throwError $ errPilotTrackNotFound ix pilot

getTaskReach :: Int -> AppT k IO [(Pilot, TrackReach)]
getTaskReach ii = do
    xs' <- fmap reachRank <$> asks masking
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "mask-track" ii

getTaskNormScore :: Int -> AppT k IO [(Pilot, Norm.NormBreakdown)]
getTaskNormScore ii = do
    xs' <- fmap Norm.score <$> asks norming
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "fs-score" ii

getTaskEffort :: Int -> AppT k IO [(Pilot, TrackEffort)]
getTaskEffort ii = do
    xs' <- fmap effortRank <$> asks landing
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "land-out" ii

getTaskArrival :: Int -> AppT k IO [(Pilot, TrackArrival)]
getTaskArrival ii = do
    xs' <- fmap arrivalRank <$> asks masking
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "mask-track" ii

getTaskLead :: Int -> AppT k IO [(Pilot, TrackLead)]
getTaskLead ii = do
    xs' <- fmap leadRank <$> asks masking
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "mask-track" ii

getTaskTime :: Int -> AppT k IO [(Pilot, TrackSpeed)]
getTaskTime ii = do
    xs' <- fmap gsSpeed <$> asks masking
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "mask-track" ii

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

errTaskLengths :: ServantErr
errTaskLengths =
    err400 {errBody = LBS.pack "I need the lengths of each task" }


errTaskStep :: String -> Int -> ServantErr
errTaskStep step ii =
    err400
        { errBody = LBS.pack
        $ "I need to have access to data from "
        ++ step
        ++ " for task: #"
        ++ show ii
        }
