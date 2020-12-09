import Debug.Trace
import Prelude hiding (abs)
import GHC.Records
import qualified Data.Text as T (Text)
import Data.Time.Clock (UTCTime)
import System.Environment (getProgName)
import Text.Printf (printf)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Data.Maybe (isNothing, catMaybes)
import Data.List ((\\), nub, sort, sortOn, find, zip4)
import qualified Data.Map.Strict as Map
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, setPort, setBeforeMainLoop)
import Servant
    ( (:<|>)(..)
    , Server, Handler(..), Proxy(..), ServantErr
    , errBody, err400, hoistServer, serve, throwError
    )
import Data.Swagger (Swagger(..), URL(..), url, info, title, version, license, description)
import Servant.Swagger
import Servant.Swagger.UI
import Network.Wai.Middleware.Gzip (gzip, def)
import System.IO (hPutStrLn, stderr)
import Control.Exception.Safe (MonadThrow, catchIO)
import Control.Lens hiding (ix)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, asks, runReaderT)
import Control.Monad.Except (ExceptT(..), MonadError)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import qualified Statistics.Sample as Stats (meanVariance)
import qualified Data.Vector as V (fromList)
import Data.Vector (Vector)
import Numeric.Sampling
import System.FilePath (takeFileName)
import System.Directory (getCurrentDirectory)

import Flight.Units ()
import Flight.Clip (FlyingSection)
import qualified Flight.Track.Cross as Cg (Crossing(..), PilotTrackCross(..))
import qualified Flight.Track.Tag as Tg (Tagging(..), PilotTrackTag(..))
import qualified Flight.Track.Stop as Sp (Framing(..))
import Flight.Track.Cross (TrackFlyingSection(..), ZoneTag(..), TrackCross(..))
import Flight.Track.Stop (StopFraming(..), TrackScoredSection(..))
import Flight.Track.Distance (TrackReach(..))
import Flight.Track.Land
    (Landing(..), TaskLanding(..), TrackEffort(..), effortRank, taskLanding)
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Lead (DiscardingLead(..), TrackLead(..))
import Flight.Track.Speed (TrackSpeed(..))
import Flight.Track.Time (TickRow(..))
import qualified Flight.Track.Mask as Mask (MaskingReach(..), MaskingArrival(..))
import Flight.Track.Mask
    ( MaskingEffort(..)
    , MaskingLead(..)
    , MaskingReach(..)
    , MaskingSpeed(..)
    )
import qualified Flight.Track.Point as Alt (AltPointing(..), AltBreakdown(..))
import Flight.Track.Point
    (Pointing(..), Velocity(..), Allocation(..), Breakdown(..))
import qualified "flight-gap-weight" Flight.Score as Wg (Weights(..))
import qualified "flight-gap-valid" Flight.Score as Vy (Validity(..))
import qualified "flight-gap-valid" Flight.Score as Vw (ValidityWorking(..))
import "flight-gap-allot" Flight.Score (PilotVelocity(..))
import "flight-gap-lead" Flight.Score (LeadingArea2Units)
import "flight-gap-math" Flight.Score (TaskPoints(..))
import "flight-gap-valid" Flight.Score
    ( DistanceValidity(..), LaunchValidity(..), TimeValidity(..)
    , TaskValidity(..), StopValidity(..)
    )
import "flight-gap-weight" Flight.Score
    ( DistanceWeight(..), ReachWeight(..), EffortWeight(..)
    , LeadingWeight(..), ArrivalWeight(..), TimeWeight(..)
    )
import Flight.Scribe
    ( readComp, readAltArrival, readAltLandout, readAltRoute, readAltScore
    , readRoute, readCrossing, readTagging, readFraming
    , readMaskingArrival
    , readMaskingEffort
    , readDiscardingLead
    , readMaskingLead
    , readMaskingReach
    , readMaskingSpeed
    , readBonusReach
    , readLanding, readFaring, readPointing
    , readPilotDiscardFurther
    )
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.ServeOptions (CmdServeOptions(..), mkOptions)
import Flight.Geodesy (EarthModel(..), EarthMath(..))
import Flight.Comp
    ( AltDot(AltFs, AltAs)
    , FindDirFile(..)
    , FileType(CompInput)
    , CompSettings(..)
    , Comp(..)
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
    , TagZoneFile(..)
    , PegFrameFile(..)
    , MaskArrivalFile(..)
    , MaskEffortFile(..)
    , MaskLeadFile(..)
    , LeadAreaFile(..)
    , MaskReachFile(..)
    , MaskSpeedFile(..)
    , BonusReachFile(..)
    , LandOutFile(..)
    , FarOutFile(..)
    , GapPointFile(..)
    , AltArrivalFile(..)
    , AltLandoutFile(..)
    , AltRouteFile(..)
    , AltScoreFile(..)
    , Pilot(..)
    , findCompInput
    , compToAltArrival
    , compToAltLandout
    , compToAltRoute
    , compToAltScore
    , compToTaskLength
    , compToCross
    , compToMaskArrival
    , compToMaskEffort
    , compToLeadArea
    , compToMaskLead
    , compToMaskReach
    , compToMaskSpeed
    , compToBonusReach
    , compToLand
    , compToFar
    , compToPoint
    , crossToTag
    , tagToPeg
    , reshape
    )
import Flight.Route
    ( OptimalRoute(..), TaskTrack(..), TrackLine(..), GeoLines(..)
    , ProjectedTrackLine(..), PlanarTrackLine(..)
    )
import Flight.Mask (checkTracks)
import Data.Ratio.Rounding (dpRound)
import Flight.Distance (QTaskDistance)
import qualified ServeOptions as Opt (description)
import ServeTrack (RawLatLngTrack(..), BolsterStats(..), crossToTrack, tagToTrack)
import ServeArea (RawLeadingArea(..))
import ServeValidity (nullValidityWorking)
import ServeSwagger (SwagUiApi)
import ServeApi

data Config k
    = Config
        { compFile :: CompInputFile
        , compSettings :: CompSettings k
        , routing :: Maybe [Maybe TaskTrack]
        , crossing :: Maybe Cg.Crossing
        , tagging :: Maybe Tg.Tagging
        , framing :: Maybe Sp.Framing
        , maskingArrival :: Maybe Mask.MaskingArrival
        , maskingEffort :: Maybe MaskingEffort
        , discardingLead2 :: Maybe (DiscardingLead LeadingArea2Units)
        , maskingLead :: Maybe (MaskingLead [u| (km^2)*s |] [u| 1/(km^2)*s |])
        , maskingReach :: Maybe MaskingReach
        , maskingSpeed :: Maybe MaskingSpeed
        , bonusReach :: Maybe MaskingReach
        , landing :: Maybe Landing
        , pointing :: Maybe Pointing
        , altFsArrival :: Maybe Mask.MaskingArrival
        , altFsLandout :: Maybe Landing
        , altFsRoute :: Maybe [GeoLines]
        , altFsScore :: Maybe Alt.AltPointing
        , altAsScore :: Maybe Alt.AltPointing
        }

nullConfig :: CompInputFile -> CompSettings k -> Config k
nullConfig cf cs =
    Config
        { compFile = cf
        , compSettings = cs
        , routing = Nothing
        , crossing = Nothing
        , tagging = Nothing
        , framing = Nothing
        , maskingArrival = Nothing
        , maskingEffort = Nothing
        , discardingLead2 = Nothing
        , maskingLead = Nothing
        , maskingReach = Nothing
        , maskingSpeed = Nothing
        , bonusReach = Nothing
        , landing = Nothing
        , pointing = Nothing
        , altFsArrival = Nothing
        , altFsLandout = Nothing
        , altFsRoute = Nothing
        , altFsScore = Nothing
        , altAsScore = Nothing
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

type CompInputSwagUiApi k = SwagUiApi :<|> CompInputApi k
type TaskLengthSwagUiApi k = SwagUiApi :<|> TaskLengthApi k
type GapPointSwagUiApi k = SwagUiApi :<|> GapPointApi k

compInputSwagUiApi :: Proxy (CompInputSwagUiApi k)
compInputSwagUiApi = Proxy

taskLengthSwagUiApi :: Proxy (TaskLengthSwagUiApi k)
taskLengthSwagUiApi = Proxy

gapPointSwagUiApi :: Proxy (GapPointSwagUiApi k)
gapPointSwagUiApi = Proxy

apiVersion :: T.Text
apiVersion = "0.25"

compInputSwagDoc :: Swagger
compInputSwagDoc = toSwagger compInputApi
  & info.title   .~ "Comp Input API"
  & info.version .~ apiVersion
  & info.description ?~ "The subset of endpoints served when only comp inputs are available."
  & info.license ?~ ("MPL" & url ?~ URL "http://mozilla.org/MPL/2.0/")

taskLengthSwagDoc :: Swagger
taskLengthSwagDoc = toSwagger taskLengthApi
  & info.title   .~ "Task Length API"
  & info.version .~ apiVersion
  & info.description ?~ "The subset of endpoints served when only comp inputs and task lengths are available."
  & info.license ?~ ("MPL" & url ?~ URL "http://mozilla.org/MPL/2.0/")

gapPointSwagDoc :: Swagger
gapPointSwagDoc = toSwagger gapPointApi
  & info.title   .~ "Gap Point API"
  & info.version .~ apiVersion
  & info.description ?~ "The full set of endpoints served when the comp has been scored."
  & info.license ?~ ("MPL" & url ?~ URL "http://mozilla.org/MPL/2.0/")

convertApp :: Config k -> AppT k IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (unApp appt) cfg

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) Opt.description Nothing

    let lf = LenientFile {coerceFile = reshape CompInput}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdServeOptions -> IO ()
drive o@CmdServeOptions{file} = do
    cwd <- getCurrentDirectory
    files <- findCompInput $ FindDirFile {dir = cwd, file = file}
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files

go :: CmdServeOptions -> CompInputFile -> IO ()
go CmdServeOptions{..} compFile@(CompInputFile compPath) = do
    let lenFile@(TaskLengthFile lenPath) = compToTaskLength compFile
    let crossFile@(CrossZoneFile crossPath) = compToCross compFile
    let tagFile@(TagZoneFile tagPath) = crossToTag crossFile
    let stopFile@(PegFrameFile stopPath) = tagToPeg tagFile
    let maskArrivalFile@(MaskArrivalFile maskArrivalPath) = compToMaskArrival compFile
    let maskEffortFile@(MaskEffortFile maskEffortPath) = compToMaskEffort compFile
    let leadAreaFile@(LeadAreaFile leadAreaPath) = compToLeadArea compFile
    let maskLeadFile@(MaskLeadFile maskLeadPath) = compToMaskLead compFile
    let maskReachFile@(MaskReachFile maskReachPath) = compToMaskReach compFile
    let maskSpeedFile@(MaskSpeedFile maskSpeedPath) = compToMaskSpeed compFile
    let bonusReachFile@(BonusReachFile bonusReachPath) = compToBonusReach compFile
    let landFile@(LandOutFile _landPath) = compToLand compFile
    let farFile@(FarOutFile landPath) = compToFar compFile
    let pointFile@(GapPointFile pointPath) = compToPoint compFile
    let altFsArrivalFile@(AltArrivalFile altFsArrivalPath) = compToAltArrival AltFs compFile
    let altFsLandoutFile@(AltLandoutFile altFsLandoutPath) = compToAltLandout AltFs compFile
    let altFsRouteFile@(AltRouteFile altFsRoutePath) = compToAltRoute AltFs compFile
    let altFsScoreFile@(AltScoreFile altFsScorePath) = compToAltScore AltFs compFile
    let altAsScoreFile@(AltScoreFile altAsScorePath) = compToAltScore AltAs compFile
    putStrLn $ "Reading task length from '" ++ takeFileName lenPath ++ "'"
    putStrLn $ "Reading competition & pilots DNF from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading flying time range from '" ++ takeFileName crossPath ++ "'"
    putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"
    putStrLn $ "Reading scored section from '" ++ takeFileName stopPath ++ "'"
    putStrLn $ "Reading arrivals from '" ++ takeFileName maskArrivalPath ++ "'"
    putStrLn $ "Reading effort from '" ++ takeFileName maskEffortPath ++ "'"
    putStrLn $ "Reading leading area from '" ++ takeFileName leadAreaPath ++ "'"
    putStrLn $ "Reading leading from '" ++ takeFileName maskLeadPath ++ "'"
    putStrLn $ "Reading reach from '" ++ takeFileName maskReachPath ++ "'"
    putStrLn $ "Reading speed from '" ++ takeFileName maskSpeedPath ++ "'"
    putStrLn $ "Reading bonus reach from '" ++ takeFileName bonusReachPath ++ "'"
    putStrLn $ "Reading land outs from '" ++ takeFileName landPath ++ "'"
    putStrLn $ "Reading scores from '" ++ takeFileName pointPath ++ "'"
    putStrLn $ "Reading FS arrivals from '" ++ takeFileName altFsArrivalPath ++ "'"
    putStrLn $ "Reading FS land outs from '" ++ takeFileName altFsLandoutPath ++ "'"
    putStrLn $ "Reading FS optimal routes from '" ++ takeFileName altFsRoutePath ++ "'"
    putStrLn $ "Reading FS scores from '" ++ takeFileName altFsScorePath ++ "'"
    putStrLn $ "Reading airScore scores from '" ++ takeFileName altAsScorePath ++ "'"

    compSettings <-
        catchIO
            (Just <$> readComp compFile)
            (const $ return Nothing)

    case compSettings of
        Nothing -> putStrLn "Couldn't read the comp settings"
        Just cs -> do
            let cfg = nullConfig compFile cs

            routes <-
                catchIO
                    (Just <$> readRoute lenFile)
                    (const $ return Nothing)

            crossing <-
                catchIO
                    (Just <$> readCrossing crossFile)
                    (const $ return Nothing)

            tagging <-
                catchIO
                    (Just <$> readTagging tagFile)
                    (const $ return Nothing)

            framing <-
                catchIO
                    (Just <$> readFraming stopFile)
                    (const $ return Nothing)

            maskingArrival <-
                catchIO
                    (Just <$> readMaskingArrival maskArrivalFile)
                    (const $ return Nothing)

            maskingEffort <-
                catchIO
                    (Just <$> readMaskingEffort maskEffortFile)
                    (const $ return Nothing)

            discardingLead2 <-
                catchIO
                    (Just <$> readDiscardingLead leadAreaFile)
                    (const $ return Nothing)

            maskingLead <-
                catchIO
                    (Just <$> readMaskingLead maskLeadFile)
                    (const $ return Nothing)

            maskingReach <-
                catchIO
                    (Just <$> readMaskingReach maskReachFile)
                    (const $ return Nothing)

            bonusReach <-
                catchIO
                    (Just <$> readBonusReach bonusReachFile)
                    (const $ return Nothing)

            maskingSpeed <-
                catchIO
                    (Just <$> readMaskingSpeed maskSpeedFile)
                    (const $ return Nothing)

            _landing <-
                catchIO
                    (Just <$> readLanding landFile)
                    (const $ return Nothing)

            landing <-
                catchIO
                    (Just <$> readFaring farFile)
                    (const $ return Nothing)

            pointing <-
                catchIO
                    (Just <$> readPointing pointFile)
                    (const $ return Nothing)

            altFsA <-
                catchIO
                    (Just <$> readAltArrival altFsArrivalFile)
                    (const $ return Nothing)

            altFsL <-
                catchIO
                    (Just <$> readAltLandout altFsLandoutFile)
                    (const $ return Nothing)

            altFsR <-
                catchIO
                    (Just <$> readAltRoute altFsRouteFile)
                    (const $ return Nothing)

            altFsS <-
                catchIO
                    (Just <$> readAltScore (traceShowId altFsScoreFile))
                    (const $ return Nothing)

            -- WARNING: Reading airScore's scores fails with
            -- AesonException "Error in $.score[0][0][1].landedMade: expected String, encountered Null"
            altAsS <-
                catchIO
                    (Just <$> readAltScore (traceShowId altAsScoreFile))
                    (const $ return Nothing)

            case (routes, crossing, tagging, framing, maskingArrival, maskingEffort, discardingLead2, maskingLead, maskingReach, maskingSpeed, bonusReach, landing, pointing) of
                (rt@(Just _), cg@(Just _), tg@(Just _), fm@(Just _), mA@(Just _), mE@(Just _), dL@(Just _), mL@(Just _), mR@(Just _), mS@(Just _), bR@(Just _), lo@(Just _), gp@(Just _)) ->
                    f =<< mkGapPointApp (Config compFile cs rt cg tg fm mA mE dL mL mR mS bR lo gp altFsA altFsL altFsR altFsS altAsS)
                (rt@(Just _), _, _, _, _, _, _, _, _, _, _, _, _) -> do
                    putStrLn "WARNING: Only serving comp inputs and task lengths"
                    f =<< mkTaskLengthApp cfg{routing = rt}
                (_, _, _, _, _, _, _, _, _, _, _, _, _) -> do
                    putStrLn "WARNING: Only serving comp inputs"
                    f =<< mkCompInputApp cfg
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
mkCompInputApp cfg = return . simpleCors . serve compInputSwagUiApi $ serverCompInputSwagUiApi cfg

mkTaskLengthApp :: Config k -> IO Application
mkTaskLengthApp cfg = return . simpleCors . serve taskLengthSwagUiApi $ serverTaskLengthSwagUiApi cfg

mkGapPointApp :: Config k -> IO Application
mkGapPointApp cfg = return . simpleCors . serve gapPointSwagUiApi $ serverGapPointSwagUiApi cfg

serverCompInputSwagUiApi :: Config k -> Server (CompInputSwagUiApi k)
serverCompInputSwagUiApi cfg =
    swaggerSchemaUIServer compInputSwagDoc :<|> serverCompInputApi cfg

serverTaskLengthSwagUiApi :: Config k -> Server (TaskLengthSwagUiApi k)
serverTaskLengthSwagUiApi cfg =
    swaggerSchemaUIServer taskLengthSwagDoc :<|> serverTaskLengthApi cfg

serverGapPointSwagUiApi :: Config k -> Server (GapPointSwagUiApi k)
serverGapPointSwagUiApi cfg =
    swaggerSchemaUIServer gapPointSwagDoc :<|> serverGapPointApi cfg

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
        :<|> getTaskRouteSphericalEdge
        :<|> getTaskRouteEllipsoidEdge
        :<|> getTaskRouteProjectedSphericalEdge
        :<|> getTaskRouteProjectedEllipsoidEdge
        :<|> getTaskRouteProjectedPlanarEdge
        :<|> getTaskRouteLengths
        :<|> getTaskPointsDiffStats
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
        :<|> getTaskPilotArea
        :<|> getTaskPilotTrackFlyingSection
        :<|> getTaskPilotTrackScoredSection
        :<|> getTaskFlyingSectionTimes
        :<|> getTaskPilotCross
        :<|> getTaskPilotTag
        :<|> getTaskBolsterStats
        :<|> getTaskBonusBolsterStats
        :<|> getTaskReach
        :<|> getTaskBonusReach
        :<|> getTaskArrival
        :<|> getTaskLead
        :<|> getTaskTime
        :<|> getTaskEffort
        :<|> getTaskLanding

        :<|> getTaskAltLanding
        :<|> getTaskAltRouteSphere
        :<|> getTaskAltRouteEllipse
        :<|> getValidity <$> n
        :<|> getAltTaskValidityWorking
        :<|> getTaskAltScore AltFs
        :<|> getTaskArrivalAlt
        :<|> getTaskAltScore AltAs
    where
        c = asks compSettings
        p = asks pointing
        n = asks altFsScore

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
        , stop = sy'
        } =
    Vy.Validity
        { launch = LaunchValidity $ dpVy ly
        , distance = DistanceValidity $ dpVy dy
        , time = TimeValidity $ dpVy ty
        , task = TaskValidity $ dpVy ky
        , stop = do
            StopValidity sy <- sy'
            return $ StopValidity $ dpVy sy
        }

roundAllocation:: Allocation -> Allocation
roundAllocation x@Allocation{..} =
    x{ weight = roundWeights weight }

getPilots :: CompSettings k -> [Pilot]
getPilots = distinctPilots . pilots

getValidity :: (HasField "validity" p [Maybe Vy.Validity]) => Maybe p -> [Maybe Vy.Validity]
getValidity Nothing = []
getValidity (Just p) = ((fmap . fmap) roundValidity) . getField @"validity" $ p

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

getTaskValidityWorking :: Int -> AppT k IO (Maybe Vw.ValidityWorking)
getTaskValidityWorking ii = do
    xs' <- fmap validityWorking <$> asks pointing
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "gap-point" ii

getAltTaskValidityWorking :: Int -> AppT k IO (Maybe Vw.ValidityWorking)
getAltTaskValidityWorking ii = do
    ls' <- fmap Alt.validityWorkingLaunch <$> asks altFsScore
    ts' <- fmap Alt.validityWorkingTime <$> asks altFsScore
    ds' <- fmap Alt.validityWorkingDistance <$> asks altFsScore
    ss' <- fmap Alt.validityWorkingStop <$> asks altFsScore
    case (ls', ts', ds', ss') of
        (Just ls, Just ts, Just ds, Just ss) ->
            case drop (ii - 1) $ zip4 ls ts ds ss of
                (lv, tv, dv, sv) : _ -> return $ do
                    lv' <- lv
                    tv' <- tv
                    dv' <- dv
                    return $
                        nullValidityWorking
                            { Vw.launch = lv'
                            , Vw.time = tv'
                            , Vw.distance = dv'
                            , Vw.stop = sv
                            }

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

getTaskPointsDiffStats :: AppT k IO [Maybe (Double, Double)]
getTaskPointsDiffStats = do
    ps <- fmap (\Pointing{score} -> (fmap . fmap . fmap) (\Breakdown{total} -> total) score) <$> asks pointing
    exs <- fmap (\Alt.AltPointing{score} -> (fmap . fmap . fmap) (\Alt.AltBreakdown{total} -> total) score) <$> asks altFsScore
    case (ps, exs) of
        (Just ps', Just exs') -> do
            let taskDiffs =
                    [
                        let exsMap = Map.fromList exsTask in
                        [(Map.lookup pilot exsMap, p') | (pilot, p') <- psTask]

                    | psTask <- ps'
                    | exsTask <- exs'
                    ]

            return $ (uncurry stats . unzip) <$> taskDiffs

        (Nothing, _) -> throwError errTaskPoints
        (_, Nothing) -> throwError errAltPoints
    where
        stats :: [Maybe TaskPoints] -> [TaskPoints] -> Maybe (Double, Double)
        stats es ps =
            let es' :: Maybe [TaskPoints]
                es' = sequence es
             in do
                    es'' <- es'
                    let xs = zipWith (\(TaskPoints p) (TaskPoints e) -> p - e) ps es''
                    (mean, variance) <- maybeMeanVariance $ V.fromList xs
                    return (mean, sqrt variance)

maybeMeanVariance :: Vector Double -> Maybe (Double, Double)
maybeMeanVariance xs = if null xs then Nothing else Just $ Stats.meanVariance xs

ellipsoidRouteLength :: TaskTrack -> Maybe (QTaskDistance Double [u| m |])
ellipsoidRouteLength
    TaskTrack
        { ellipsoidEdgeToEdge =
            OptimalRoute{taskRoute = Just TrackLine{distance = d}}} = Just d
ellipsoidRouteLength
    TaskTrack
        { ellipsoidEdgeToEdge =
            OptimalRoute{taskRoute = Nothing}} = Nothing

flatRouteLength :: TaskTrack -> Maybe (QTaskDistance Double [u| m |])
flatRouteLength
    TaskTrack
        { projection =
            OptimalRoute
                { taskRoute =
                    Just
                        ProjectedTrackLine
                            {spherical =
                                TrackLine
                                    {distance = d}}}} = Just d
flatRouteLength
    TaskTrack
        { projection = OptimalRoute{taskRoute = Nothing} } = Nothing

getRouteLength
    :: EarthModel Double
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
getRouteLength (EarthAsEllipsoid _) Vincenty track = ellipsoidRouteLength track
getRouteLength (EarthAsEllipsoid _) AndoyerLambert track = ellipsoidRouteLength track
getRouteLength (EarthAsEllipsoid _) ForsytheAndoyerLambert track = ellipsoidRouteLength track
getRouteLength (EarthAsEllipsoid _) FsAndoyer track = ellipsoidRouteLength track
getRouteLength (EarthAsFlat _) Pythagorus track = flatRouteLength track
getRouteLength _ _ _ = Nothing

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

getTaskPilotArea :: Int -> String -> AppT k IO RawLeadingArea
getTaskPilotArea ii pilotId = do
    let jj = ii - 1
    let ix = IxTask ii
    let pilot = PilotId pilotId
    cf <- asks compFile
    ml <- asks maskingLead
    dl <- asks discardingLead2
    ps <- getPilots <$> asks compSettings
    let p = find (\(Pilot (pid, _)) -> pid == pilot) ps

    case (p, ml, dl) of
        (Nothing, _, _) -> throwError $ errPilotNotFound pilot
        (_, Nothing, _) -> throwError $ errPilotNotFound pilot
        (_, _, Nothing) -> throwError $ errPilotNotFound pilot
        (Just p'
            , Just MaskingLead{raceTime = rt, raceDistance = rd}
            , Just DiscardingLead{areas = sq}) -> do

            xs <-
                liftIO $ catchIO
                    (Just <$> readPilotDiscardFurther cf ix p')
                    (const $ return Nothing)

            case (xs, take 1 $ drop jj rt, take 1 $ drop jj rd, take 1 $ drop jj sq) of
                (Nothing, _, _, _) -> throwError $ errPilotTrackNotFound ix pilot
                (Just _, [], _, _) -> throwError $ errPilotTrackNotFound ix pilot
                (Just _, _, [], _) -> throwError $ errPilotTrackNotFound ix pilot
                (Just _, _, _, []) -> throwError $ errPilotTrackNotFound ix pilot
                (Just [], _, _, _) -> throwError $ errPilotTrackNotFound ix pilot
                (Just (x0 : xs1toN), t : _, d : _, areas : _) ->
                    case find (\(p'', _) -> p'' == p') areas of
                        Nothing -> throwError $ errPilotTrackNotFound ix pilot
                        Just (_, a) ->
                            case reverse xs1toN of
                                [] -> return $ RawLeadingArea t d (x0 : xs1toN) a
                                (xsN : xs2toM) -> do
                                    ys <- liftIO $ resampleIO 500 xs2toM
                                    let zs = x0 : (sortOn fixIdx ys) ++ [xsN]
                                    return $ RawLeadingArea t d (nub zs) a

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

getTaskPilotTrackScoredSection :: Int -> String -> AppT k IO TrackScoredSection
getTaskPilotTrackScoredSection ii pilotId = do
    let jj = ii - 1
    let ix = IxTask ii
    let pilot = PilotId pilotId
    fss <- fmap Sp.stopFlying <$> asks framing
    let isPilot (Pilot (pid, _)) = pid == PilotId pilotId

    case fss of
        Nothing -> throwError $ errPilotNotFound pilot
        Just fss' -> do
            case take 1 $ drop jj fss' of
                fs : _ ->
                    case find (isPilot . fst) fs of
                        Just (_, StopFraming{stopScored = Just y}) -> return y
                        _ -> throwError $ errPilotTrackNotFound ix pilot
                _ -> throwError $ errPilotTrackNotFound ix pilot

getTaskFlyingSectionTimes :: Int -> AppT k IO [(Pilot, FlyingSection UTCTime)]
getTaskFlyingSectionTimes ii = do
    let jj = ii - 1
    fss <- fmap Cg.flying <$> asks crossing

    case fss of
        Just fss' -> do
            case take 1 $ drop jj fss' of
                fs : _ ->
                    -- NOTE: Sort by landing time.
                    return . sortOn (fmap snd . snd) . catMaybes $
                        -- NOTE: Use of sequence on a tuple.
                        -- > sequence (1, Nothing)
                        -- Nothing
                        -- > sequence (1, Just 2)
                        -- Just (1, 2)
                        [sequence pt | pt <- (fmap . fmap . fmap) flyingTimes fs]
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "cross-zone" ii

getTaskPilotCross :: Int -> String -> AppT k IO (Maybe TrackCross)
getTaskPilotCross ii pilotId = do
    let jj = ii - 1
    let ix = IxTask ii
    let pilot = PilotId pilotId
    fss <- fmap Cg.crossing <$> asks crossing
    let isPilot (Cg.PilotTrackCross (Pilot (pid, _)) _) = pid == PilotId pilotId

    case fss of
        Nothing -> throwError $ errPilotNotFound pilot
        Just fss' -> do
            case take 1 $ drop jj fss' of
                fs : _ ->
                    case find isPilot fs of
                        Just y -> return $ crossToTrack y
                        _ -> throwError $ errPilotTrackNotFound ix pilot
                _ -> throwError $ errPilotTrackNotFound ix pilot

getTaskPilotTag :: Int -> String -> AppT k IO [Maybe ZoneTag]
getTaskPilotTag ii pilotId = do
    let jj = ii - 1
    let ix = IxTask ii
    let pilot = PilotId pilotId
    fss <- fmap Tg.tagging <$> asks tagging
    let isPilot (Tg.PilotTrackTag (Pilot (pid, _)) _) = pid == PilotId pilotId

    case fss of
        Nothing -> throwError $ errPilotNotFound pilot
        Just fss' -> do
            case take 1 $ drop jj fss' of
                fs : _ ->
                    case find isPilot fs of
                        Just y -> return $ tagToTrack y
                        _ -> throwError $ errPilotTrackNotFound ix pilot
                _ -> throwError $ errPilotTrackNotFound ix pilot

getTaskBolsterStats :: Int -> AppT k IO BolsterStats
getTaskBolsterStats ii = do
    bs' <- fmap Mask.bolster <$> asks maskingReach
    rs' <- fmap Mask.reach <$> asks maskingReach
    case (bs', rs') of
        (Just bs, Just rs) ->
            let f = drop (ii - 1) in
            case (f bs, f rs) of
                (b : _, r : _) -> return BolsterStats{bolster = b, reach = r}
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "mask-track" ii

getTaskBonusBolsterStats :: Int -> AppT k IO BolsterStats
getTaskBonusBolsterStats ii = do
    bs' <- fmap Mask.bolster <$> asks bonusReach
    rs' <- fmap Mask.reach <$> asks bonusReach
    case (bs', rs') of
        (Just bs, Just rs) ->
            let f = drop (ii - 1) in
            case (f bs, f rs) of
                (b : _, r : _) -> return BolsterStats{bolster = b, reach = r}
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "mask-track" ii

getTaskReach :: Int -> AppT k IO [(Pilot, TrackReach)]
getTaskReach ii = do
    xs' <- fmap reachRank <$> asks maskingReach
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "mask-track" ii

getTaskBonusReach :: Int -> AppT k IO [(Pilot, TrackReach)]
getTaskBonusReach ii = do
    xs' <- fmap reachRank <$> asks bonusReach
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "mask-track" ii

getTaskAltScore :: AltDot -> Int -> AppT k IO [(Pilot, Alt.AltBreakdown)]
getTaskAltScore altDot ii = do
    let (altScore, altSegment) =
            case altDot of
                AltFs -> (altFsScore, "fs-score")
                AltAs -> (altAsScore, "as-score")

    xs' <- fmap Alt.score <$> asks altScore
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep altSegment ii

getTaskAltRouteSphere :: Int -> AppT k IO (Maybe TrackLine)
getTaskAltRouteSphere ii = do
    xs' <- asks altFsRoute
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return $ sphere x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "fs-route" ii

getTaskAltRouteEllipse :: Int -> AppT k IO (Maybe TrackLine)
getTaskAltRouteEllipse ii = do
    xs' <- asks altFsRoute
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return $ ellipse x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "fs-route" ii

getTaskEffort :: Int -> AppT k IO [(Pilot, TrackEffort)]
getTaskEffort ii = do
    xs' <- fmap effortRank <$> asks landing
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "land-out" ii

getTaskAltLanding :: Int -> AppT k IO (Maybe TaskLanding)
getTaskAltLanding ii = do
    x <- asks altFsLandout
    return . join $ taskLanding (IxTask ii) <$> x

getTaskLanding :: Int -> AppT k IO (Maybe TaskLanding)
getTaskLanding ii = do
    x <- asks landing
    return . join $ taskLanding (IxTask ii) <$> x

getTaskArrivalAlt :: Int -> AppT k IO [(Pilot, TrackArrival)]
getTaskArrivalAlt ii = do
    xs' <- fmap Mask.arrivalRank <$> asks maskingArrival
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "mask-track" ii

getTaskArrival :: Int -> AppT k IO [(Pilot, TrackArrival)]
getTaskArrival ii = do
    xs' <- fmap Mask.arrivalRank <$> asks maskingArrival
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "mask-track" ii

getTaskLead :: Int -> AppT k IO [(Pilot, TrackLead LeadingArea2Units)]
getTaskLead ii = do
    xs' <- fmap leadRank <$> asks maskingLead
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "mask-track" ii

getTaskTime :: Int -> AppT k IO [(Pilot, TrackSpeed)]
getTaskTime ii = do
    xs' <- fmap gsSpeed <$> asks maskingSpeed
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

errTaskPoints :: ServantErr
errTaskPoints =
    err400 {errBody = LBS.pack "I need the points of each task" }

errAltPoints :: ServantErr
errAltPoints =
    err400 {errBody = LBS.pack "I need the expected points of each task" }

errTaskStep :: String -> Int -> ServantErr
errTaskStep step ii =
    err400
        { errBody = LBS.pack
        $ "I need to have access to data from "
        ++ step
        ++ " for task: #"
        ++ show ii
        }
