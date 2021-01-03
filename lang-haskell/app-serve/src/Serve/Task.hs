module Serve.Task
    ( getTaskScore
    , getTaskValidityWorking
    , getTaskFlyingSectionTimes
    , getTaskBolsterStats
    , getTaskBonusBolsterStats
    , getTaskReach
    , getTaskBonusReach
    , getTaskEffort
    , getTaskLanding
    , getTaskArrival
    , getTaskLead
    , getTaskTime
    , getValidity
    , getAllocation
    ) where

import GHC.Records
import Data.Time.Clock (UTCTime)
import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Servant (throwError)
import Control.Monad (join)
import Control.Monad.Reader (asks)

import Flight.Units ()
import Flight.Clip (FlyingSection)
import qualified Flight.Track.Cross as Cg (CompFlying(..))
import Flight.Track.Cross (TrackFlyingSection(..))
import Flight.Track.Distance (TrackReach(..))
import Flight.Track.Land
    (TaskLanding(..), TrackEffort(..), effortRank, taskLanding)
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Lead (TrackLead(..))
import Flight.Track.Speed (TrackSpeed(..))
import qualified Flight.Track.Mask as Mask (CompMaskingReach(..), CompMaskingArrival(..))
import Flight.Track.Mask
    ( MaskingLead(..)
    , CompMaskingReach(..)
    , MaskingSpeed(..)
    )
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
import Flight.Comp (IxTask(..), Pilot(..))
import Data.Ratio.Rounding (dpRound)
import Serve.Track (BolsterStats(..))
import Serve.Config (AppT(..), Config(..))
import Serve.Error (errTaskStep, errTaskBounds)
import Serve.Pilot (getScores)

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

getTaskFlyingSectionTimes :: Int -> AppT k IO [(Pilot, FlyingSection UTCTime)]
getTaskFlyingSectionTimes ii = do
    let jj = ii - 1
    fss <- fmap Cg.flying <$> asks flying

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

getTaskEffort :: Int -> AppT k IO [(Pilot, TrackEffort)]
getTaskEffort ii = do
    xs' <- fmap effortRank <$> asks landing
    case xs' of
        Just xs ->
            case drop (ii - 1) xs of
                x : _ -> return x
                _ -> throwError $ errTaskBounds ii

        _ -> throwError $ errTaskStep "land-out" ii

getTaskLanding :: Int -> AppT k IO (Maybe TaskLanding)
getTaskLanding ii = do
    x <- asks landing
    return . join $ taskLanding (IxTask ii) <$> x

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
