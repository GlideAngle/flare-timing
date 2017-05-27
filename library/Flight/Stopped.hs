{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Flight.Stopped
    ( TaskStopTime(..)
    , AnnouncedTime(..)
    , ScoreBackTime(..)
    , StartGateInterval(..)
    , StopTime(..)
    , NumberInGoalAtStop(..)
    , CanScoreStopped(..)
    , stopTaskTime
    , canScoreStopped
    , PilotsLaunched
    , PilotsLandedBeforeStop
    , DistanceLaunchToEss
    , DistanceFlown
    , StoppedValidity
    , stoppedValidity
    , TaskType(..)
    , StartGates(..)
    , ScoreTimeWindow(..)
    , scoreTimeWindow
    , AltitudeAboveGoal
    , DistanceToGoal
    , GlideRatio
    , StoppedTrack
    , applyGlide
    ) where

import Data.Ratio ((%))
import Statistics.Sample (mean, stdDev)
import Control.Arrow (second)
import Data.List (partition, sortBy)
import qualified Data.Vector as V
import qualified Data.Map as Map

import Flight.Points (Hg, Pg)
import Flight.Leading (TaskTime(..))

newtype TaskStopTime = TaskStopTime Rational deriving (Eq, Ord, Show)
newtype AnnouncedTime = AnnouncedTime Rational deriving (Eq, Ord, Show)
newtype ScoreBackTime = ScoreBackTime Rational deriving (Eq, Ord, Show)
newtype StartGateInterval = StartGateInterval Rational deriving (Eq, Ord, Show)

data StopTime a where
    ScoreBackStop :: ScoreBackTime -> AnnouncedTime -> StopTime Pg
    InterGateStop :: StartGateInterval -> AnnouncedTime -> StopTime Hg
    SingleGateStop :: AnnouncedTime -> StopTime Hg

stopTaskTime :: forall a. StopTime a -> TaskStopTime
stopTaskTime (ScoreBackStop (ScoreBackTime sb) (AnnouncedTime at)) =
    TaskStopTime $ at - sb
stopTaskTime (InterGateStop (StartGateInterval sgi) (AnnouncedTime at)) =
    TaskStopTime $ at - sgi
stopTaskTime (SingleGateStop (AnnouncedTime at)) = 
    TaskStopTime $ at - ((15 * 60) % 1)

newtype NumberInGoalAtStop = NumberInGoalAtStop Int deriving (Eq, Ord, Show)
    
data CanScoreStopped a where
    Womens :: NumberInGoalAtStop -> TaskStopTime -> CanScoreStopped Hg
    GoalOrDuration :: NumberInGoalAtStop -> TaskStopTime -> CanScoreStopped Hg
    FromGetGo :: TaskStopTime -> CanScoreStopped Pg
    FromLastStart :: [TaskTime] -> TaskStopTime -> CanScoreStopped Pg

deriving instance Show (CanScoreStopped a)

canScoreStopped :: forall a. CanScoreStopped a -> Bool
canScoreStopped (Womens (NumberInGoalAtStop n) (TaskStopTime t)) =
    n > 0 || t > 60 * 60
canScoreStopped (GoalOrDuration (NumberInGoalAtStop n) (TaskStopTime t)) =
    n > 0 || t > 90 * 60
canScoreStopped (FromGetGo (TaskStopTime t)) =
    t > 60 * 60
canScoreStopped (FromLastStart [] _) =
    False
canScoreStopped (FromLastStart xs (TaskStopTime t)) =
    (t - lastStart) > 60 * 60
    where
        lastStart = minimum $ (\(TaskTime x) -> x) <$> xs

newtype PilotsMakingEss = PilotsMakingEss Int deriving (Eq, Ord, Show)
newtype StoppedValidity = StoppedValidity Rational deriving (Eq, Show)
newtype DistanceFlown = DistanceFlown Rational deriving (Eq, Show)
newtype DistanceLaunchToEss = DistanceLaunchToEss Rational deriving (Eq, Show)
newtype PilotsLandedBeforeStop = PilotsLandedBeforeStop Int deriving (Eq, Show)
newtype PilotsLaunched = PilotsLaunched Int deriving (Eq, Show)

stoppedValidity :: PilotsLaunched
                   -> PilotsLandedBeforeStop
                   -> DistanceLaunchToEss
                   -> [DistanceFlown]
                   -> StoppedValidity
stoppedValidity _ _ _ [] =
    StoppedValidity 0
stoppedValidity
    (PilotsLaunched launched)
    (PilotsLandedBeforeStop landed)
    (DistanceLaunchToEss dist)
    xs =
    StoppedValidity $ min 1 validity
    where
        validity :: Rational
        validity = toRational $ (a * b) ** (1 / 2) + c ** 3

        numerator :: Double
        numerator = maxFlown - zsMean

        denominator :: Double 
        denominator = fromRational dist - maxFlown + 1

        a :: Double 
        a = numerator / denominator

        b :: Double 
        b = (zsStdDev / 5) ** (1 / 2)

        c :: Double 
        c = (fromIntegral landed / fromIntegral launched) ** (1 / 3)

        ys :: [Rational]
        ys = (\(DistanceFlown x) -> x) <$> xs

        zs :: [Double]
        zs = fromRational <$> ys

        vs = V.fromList zs

        zsMean :: Double
        zsMean = mean vs

        zsStdDev :: Double
        zsStdDev = stdDev vs

        maxFlown :: Double
        maxFlown = fromRational $ maximum ys

newtype StartGates = StartGates Int deriving (Eq, Ord, Show)
data TaskType = RaceToGoal | ElapsedTime deriving (Eq, Ord, Show)
newtype ScoreTimeWindow = ScoreTimeWindow Rational deriving (Eq, Ord, Show)

scoreTimeWindow :: TaskType
                     -> StartGates
                     -> TaskStopTime
                     -> [TaskTime] -> ScoreTimeWindow
scoreTimeWindow RaceToGoal (StartGates 1) (TaskStopTime stopTime) _ = 
    ScoreTimeWindow stopTime
scoreTimeWindow _ _ (TaskStopTime stopTime) ts = 
    ScoreTimeWindow $ stopTime - lastStart
    where
        lastStart = maximum $ (\(TaskTime t) -> t) <$> ts

-- | GPS altitude. TODO: State the units for altitude. Is it feet or metres?
newtype AltitudeAboveGoal = AltitudeAboveGoal Rational deriving (Eq, Ord, Show)
--
-- | The distance in km to goal.
newtype DistanceToGoal = DistanceToGoal Rational deriving (Eq, Ord, Show)

newtype GlideRatio = GlideRatio Rational deriving (Eq, Ord, Show)

newtype StoppedTrack = StoppedTrack [(TaskTime, DistanceToGoal)] deriving (Eq, Ord, Show)

madeGoal :: StoppedTrack -> Bool
madeGoal (StoppedTrack xs) =
    any (\(DistanceToGoal d) -> d <= 0) $ snd <$> xs

applyGlide :: GlideRatio -> [AltitudeAboveGoal] -> [StoppedTrack] -> [StoppedTrack]
applyGlide (GlideRatio gr) alts xs =
    snd <$> ysSorted
    where
        iXs :: [(Int, StoppedTrack)]
        iXs = zip [1 .. ] xs

        altMap = Map.fromList $ zip [1 .. ] alts

        (xsMadeGoal :: [(Int, StoppedTrack)], xsLandedOut :: [(Int, StoppedTrack)]) =
            partition
                (\(_, track) -> madeGoal track)
                iXs

        glide :: AltitudeAboveGoal -> DistanceToGoal -> DistanceToGoal
        glide (AltitudeAboveGoal alt) (DistanceToGoal dtg) =
            DistanceToGoal $ min 0 (dtg - alt * gr)

        glides d@(i, StoppedTrack track) =
            case Map.lookup i altMap of
                 Nothing -> d
                 Just alt -> (i, StoppedTrack $ second (glide alt) <$> track)

        ysLandedOut = glides <$> xsLandedOut
        
        ysMerged :: [(Int, StoppedTrack)]
        ysMerged = mconcat [ xsMadeGoal, ysLandedOut ]

        ysSorted :: [(Int, StoppedTrack)]
        ysSorted = sortBy (\x y -> fst x `compare` fst y) ysMerged
