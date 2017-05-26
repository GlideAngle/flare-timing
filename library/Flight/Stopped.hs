{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Flight.Stopped where

import Data.Ratio ((%))

import Flight.Points (Hg, Pg)

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
