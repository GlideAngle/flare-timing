module Serve.App
    ( apiVersion
    , compInputSwagUiApi
    , taskLengthSwagUiApi
    , gapPointSwagUiApi
    , compInputSwagDoc
    , taskLengthSwagDoc
    , gapPointSwagDoc
    , serverCompInputSwagUiApi
    , serverTaskLengthSwagUiApi
    , serverGapPointSwagUiApi
    , serverCompInputApi
    , serverTaskLengthApi
    , serverGapPointApi
    , convertApp
    , mkCompInputApp
    , mkTaskLengthApp
    , mkGapPointApp
    ) where

import GHC.Records
import qualified Data.Text as T (Text)
import Data.Time.Clock (UTCTime)
import Data.Maybe (catMaybes)
import Data.List (sortOn, zip4)
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
    ( (:<|>)(..)
    , Server, Handler(..), Proxy(..)
    , hoistServer, serve, throwError
    )
import Data.Swagger (Swagger(..), URL(..), url, info, title, version, license, description)
import Servant.Swagger
import Servant.Swagger.UI
import Control.Lens hiding (ix)
import Control.Monad (join)
import Control.Monad.Reader (asks, runReaderT)

import Flight.Units ()
import Flight.Clip (FlyingSection)
import qualified Flight.Track.Cross as Cg (Crossing(..))
import Flight.Track.Cross (TrackFlyingSection(..))
import Flight.Track.Distance (TrackReach(..))
import Flight.Track.Land
    (TaskLanding(..), TrackEffort(..), effortRank, taskLanding)
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Lead (TrackLead(..))
import Flight.Track.Speed (TrackSpeed(..))
import qualified Flight.Track.Mask as Mask (MaskingReach(..), MaskingArrival(..))
import Flight.Track.Mask
    ( MaskingLead(..)
    , MaskingReach(..)
    , MaskingSpeed(..)
    )
import qualified Flight.Track.Point as Alt (AltPointing(..), AltBreakdown(..))
import Flight.Track.Point (Pointing(..), Allocation(..), Breakdown(..))
import qualified "flight-gap-weight" Flight.Score as Wg (Weights(..))
import qualified "flight-gap-valid" Flight.Score as Vy (Validity(..))
import qualified "flight-gap-valid" Flight.Score as Vw (ValidityWorking(..))
import "flight-gap-lead" Flight.Score (LeadingArea2Units)
import "flight-gap-valid" Flight.Score
    ( DistanceValidity(..), LaunchValidity(..), TimeValidity(..)
    , TaskValidity(..), StopValidity(..)
    )
import "flight-gap-weight" Flight.Score
    ( DistanceWeight(..), ReachWeight(..), EffortWeight(..)
    , LeadingWeight(..), ArrivalWeight(..), TimeWeight(..)
    )
import Flight.Comp (AltDot(AltFs, AltAs), IxTask(..), Pilot(..), CompSettings(..))
import Flight.Route (TrackLine(..), GeoLines(..))
import Data.Ratio.Rounding (dpRound)
import Serve.Track (BolsterStats(..))
import Serve.Validity (nullValidityWorking)
import ServeSwagger (SwagUiApi)
import Serve.Config (AppT(..), Config(..))
import Serve.Api
    ( CompInputApi, TaskLengthApi, GapPointApi
    , compInputApi, taskLengthApi, gapPointApi
    )
import Serve.Error (errTaskStep, errTaskBounds)
import Serve.PointDiff (getTaskPointsDiffStats)
import Serve.Route
    ( getTaskRouteSphericalEdge
    , getTaskRouteEllipsoidEdge
    , getTaskRouteProjectedSphericalEdge
    , getTaskRouteProjectedEllipsoidEdge
    , getTaskRouteProjectedPlanarEdge
    , getTaskRouteLengths
    )
import Serve.Pilot
    ( getTaskPilotAbs
    , getTaskPilotDnf
    , getTaskPilotDfNoTrack
    , getTaskPilotNyp
    , getTaskPilotDf
    , getTaskPilotTrack
    , getTaskPilotArea
    , getTaskPilotTrackFlyingSection
    , getTaskPilotTrackScoredSection
    , getTaskPilotCross
    , getTaskPilotTag
    , getPilots
    , getPilotsStatus
    , getScores
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

getValidity :: (HasField "validity" p [Maybe Vy.Validity]) => Maybe p -> [Maybe Vy.Validity]
getValidity Nothing = []
getValidity (Just p) = ((fmap . fmap) roundValidity) . getField @"validity" $ p

getAllocation :: Maybe Pointing -> [Maybe Allocation]
getAllocation Nothing = []
getAllocation (Just p) = ((fmap . fmap) roundAllocation) . allocation $ p

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
