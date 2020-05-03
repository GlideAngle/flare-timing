module Flight.Gap.Stopped
    ( TaskStopTime(..)
    , AnnouncedTime(..)
    , ScoreBackTime(..)
    , StartGateInterval(..)
    , StopTime(..)
    , CanScoreStopped(..)
    , stopTaskTime
    , canScoreStopped
    , TaskType(..)
    , StartGates(..)
    , ScoreTimeWindow(..)
    , scoreTimeWindow
    , AltitudeAboveGoal(..)
    , DistanceToGoal(..)
    , GlideRatio(..)
    , StoppedTrack(..)
    , applyGlide
    , applyGlides
    ) where

import Data.Ratio ((%))
import Control.Arrow (second)
import Data.List (partition, sortBy)
import qualified Data.Map as Map
import Data.UnitsOfMeasure (u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import "flight-gap-allot" Flight.Score (PilotsAtEss(..))
import "flight-gap-lead" Flight.Score (TaskTime(..))
import "flight-gap-math" Flight.Score (Hg, Pg)
import Flight.Gap.Time.ScoreBack (ScoreBackTime(..))

newtype TaskStopTime = TaskStopTime Rational deriving (Eq, Ord, Show)
newtype AnnouncedTime = AnnouncedTime Rational deriving (Eq, Ord, Show)
newtype StartGateInterval = StartGateInterval Rational deriving (Eq, Ord, Show)

data StopTime a where
    ScoreBackStop
        :: ScoreBackTime (Quantity Double [u| s |])
        -> AnnouncedTime
        -> StopTime Pg

    InterGateStop :: StartGateInterval -> AnnouncedTime -> StopTime Hg
    SingleGateStop :: AnnouncedTime -> StopTime Hg

deriving instance Show (StopTime a)

stopTaskTime :: forall a. StopTime a -> TaskStopTime
stopTaskTime (ScoreBackStop (ScoreBackTime sb) (AnnouncedTime at)) =
    TaskStopTime $ at - sb'
    where
        (MkQuantity sb') :: Quantity Rational [u| s |] =
            convert . toRational' $ sb

stopTaskTime (InterGateStop (StartGateInterval sgi) (AnnouncedTime at)) =
    TaskStopTime $ at - sgi

stopTaskTime (SingleGateStop (AnnouncedTime at)) = 
    TaskStopTime $ at - ((15 * 60) % 1)

data CanScoreStopped a where
    Womens :: PilotsAtEss -> TaskStopTime -> CanScoreStopped Hg
    GoalOrDuration :: PilotsAtEss -> TaskStopTime -> CanScoreStopped Hg
    FromGetGo :: TaskStopTime -> CanScoreStopped Pg
    FromLastStart :: [TaskTime] -> TaskStopTime -> CanScoreStopped Pg

deriving instance Show (CanScoreStopped a)

canScoreStopped :: forall a. CanScoreStopped a -> Bool
canScoreStopped (Womens (PilotsAtEss n) (TaskStopTime t)) =
    n > 0 || t >= 60 * 60
canScoreStopped (GoalOrDuration (PilotsAtEss n) (TaskStopTime t)) =
    n > 0 || t >= 90 * 60
canScoreStopped (FromGetGo (TaskStopTime t)) =
    t >= 60 * 60
canScoreStopped (FromLastStart [] _) =
    False
canScoreStopped (FromLastStart xs (TaskStopTime t)) =
    (t - lastStart) >= 60 * 60
    where
        lastStart = maximum $ (\(TaskTime (MkQuantity x)) -> x) <$> xs

newtype StartGates = StartGates Int deriving (Eq, Ord, Show)
data TaskType = RaceToGoal | ElapsedTime deriving (Eq, Ord, Show)

newtype ScoreTimeWindow = ScoreTimeWindow Rational deriving (Eq, Ord, Show)

scoreTimeWindow
    :: TaskType
    -> StartGates
    -> TaskStopTime
    -> [TaskTime] -> ScoreTimeWindow

scoreTimeWindow RaceToGoal (StartGates 1) (TaskStopTime stopTime) _ = 
    ScoreTimeWindow stopTime
scoreTimeWindow _ _ _ [] =
    ScoreTimeWindow 0
scoreTimeWindow _ _ (TaskStopTime stopTime) xs
    | any (> stopTime) ys =
        ScoreTimeWindow 0
    | otherwise =
        ScoreTimeWindow $ stopTime - maximum ys
    where
        ys = (\(TaskTime (MkQuantity t)) -> t) <$> xs

-- | GPS altitude. TODO: State the units for altitude. Is it feet or metres?
newtype AltitudeAboveGoal = AltitudeAboveGoal Rational deriving (Eq, Ord, Show)

-- | The distance in km to goal.
newtype DistanceToGoal = DistanceToGoal Rational deriving (Eq, Ord, Show)

newtype GlideRatio = GlideRatio Rational deriving (Eq, Ord, Show)

newtype StoppedTrack = StoppedTrack [(TaskTime, DistanceToGoal)] deriving (Eq, Ord, Show)

madeGoal :: StoppedTrack -> Bool
madeGoal (StoppedTrack xs) =
    any (\(DistanceToGoal d) -> d <= 0) $ snd <$> xs

applyGlide :: GlideRatio -> AltitudeAboveGoal -> StoppedTrack -> StoppedTrack
applyGlide (GlideRatio gr) altitude x@(StoppedTrack track)
    | gr <= 0 = x
    | otherwise =
        StoppedTrack $ second (glide altitude) <$> track
            where
                glide :: AltitudeAboveGoal -> DistanceToGoal -> DistanceToGoal
                glide (AltitudeAboveGoal alt) (DistanceToGoal dtg) =
                    DistanceToGoal $ if dtg > bonus then dtg - bonus else 0
                    where
                        bonus = alt * gr

applyGlides :: GlideRatio -> [AltitudeAboveGoal] -> [StoppedTrack] -> [StoppedTrack]
applyGlides gr@(GlideRatio glideRatio) alts xs
    | glideRatio <= 0 = xs
    | otherwise =
        snd <$> ysSorted
        where
            iXs :: [(Int, StoppedTrack)]
            iXs = zip [1 .. ] xs

            altMap =
                Map.fromList
                $ filter (\(_, AltitudeAboveGoal alt) -> alt > 0)
                $ zip [1 .. ] alts

            (xsMadeGoal :: [(Int, StoppedTrack)], xsLandedOut :: [(Int, StoppedTrack)]) =
                partition
                    (\(_, track) -> madeGoal track)
                    iXs

            glides d@(i, track) =
                case Map.lookup i altMap of
                     Nothing -> d
                     Just alt -> (i, applyGlide gr alt track)

            ysLandedOut = glides <$> xsLandedOut

            ysMerged :: [(Int, StoppedTrack)]
            ysMerged = mconcat [ xsMadeGoal, ysLandedOut ]

            ysSorted :: [(Int, StoppedTrack)]
            ysSorted = sortBy (\x y -> fst x `compare` fst y) ysMerged
