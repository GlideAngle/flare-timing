{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
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
    ) where

import Data.Ratio ((%))
import Statistics.Sample (mean, stdDev)
import qualified Data.Vector as V

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
        denominator = (fromRational dist) - maxFlown + 1

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
