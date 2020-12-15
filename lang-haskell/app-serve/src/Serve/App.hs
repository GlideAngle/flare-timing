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
import Data.List ((\\), nub, sort, sortOn, find, zip4)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
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
import Control.Exception.Safe (catchIO)
import Control.Lens hiding (ix)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks, runReaderT)
import Numeric.Sampling

import Flight.Units ()
import Flight.Clip (FlyingSection)
import qualified Flight.Track.Cross as Cg (Crossing(..), PilotTrackCross(..))
import qualified Flight.Track.Tag as Tg (Tagging(..), PilotTrackTag(..))
import qualified Flight.Track.Stop as Sp (Framing(..))
import Flight.Track.Cross (TrackFlyingSection(..), ZoneTag(..), TrackCross(..))
import Flight.Track.Stop (StopFraming(..), TrackScoredSection(..))
import Flight.Track.Distance (TrackReach(..))
import Flight.Track.Land
    (TaskLanding(..), TrackEffort(..), effortRank, taskLanding)
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Lead (DiscardingLead(..), TrackLead(..))
import Flight.Track.Speed (TrackSpeed(..))
import Flight.Track.Time (TickRow(..))
import qualified Flight.Track.Mask as Mask (MaskingReach(..), MaskingArrival(..))
import Flight.Track.Mask
    ( MaskingLead(..)
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
import "flight-gap-valid" Flight.Score
    ( DistanceValidity(..), LaunchValidity(..), TimeValidity(..)
    , TaskValidity(..), StopValidity(..)
    )
import "flight-gap-weight" Flight.Score
    ( DistanceWeight(..), ReachWeight(..), EffortWeight(..)
    , LeadingWeight(..), ArrivalWeight(..), TimeWeight(..)
    )
import Flight.Scribe (readPilotDiscardFurther)
import Flight.Comp
    ( AltDot(AltFs, AltAs)
    , IxTask(..)
    , Pilot(..)
    , PilotId(..)
    , PilotGroup(..)
    , PilotTaskStatus(..)
    , PilotTrackLogFile(..)
    , DfNoTrackPilot(..)
    , DfNoTrack(..)
    , Nyp(..)
    , CompSettings(..)
    )
import Flight.Route (TrackLine(..), GeoLines(..))
import Flight.Mask (checkTracks)
import Data.Ratio.Rounding (dpRound)
import Serve.Track (RawLatLngTrack(..), BolsterStats(..), crossToTrack, tagToTrack)
import Serve.Area (RawLeadingArea(..))
import Serve.Validity (nullValidityWorking)
import ServeSwagger (SwagUiApi)
import Serve.Config (AppT(..), Config(..))
import Serve.Api
    ( CompInputApi, TaskLengthApi, GapPointApi
    , compInputApi, taskLengthApi, gapPointApi
    )
import Serve.Error (errTaskStep, errTaskBounds, errPilotNotFound, errPilotTrackNotFound)
import Serve.PointDiff (getTaskPointsDiffStats)
import Serve.Route
    ( getTaskRouteSphericalEdge
    , getTaskRouteEllipsoidEdge
    , getTaskRouteProjectedSphericalEdge
    , getTaskRouteProjectedEllipsoidEdge
    , getTaskRouteProjectedPlanarEdge
    , getTaskRouteLengths
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
