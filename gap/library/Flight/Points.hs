{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Flight.Points
    ( LaunchToSssPoints(..)
    , MinimumDistancePoints(..)
    , SecondsPerPoint(..)
    , JumpedTheGun(..)
    , Hg
    , Pg
    , Penalty(..)
    , TaskPointParts(..)
    , TaskPoints(..)
    , zeroPoints
    , taskPoints
    , applyPointPenalty
    ) where

import Data.Ratio ((%))

newtype LaunchToSssPoints = LaunchToSssPoints Rational deriving (Eq, Show)
newtype MinimumDistancePoints = MinimumDistancePoints Rational deriving (Eq, Show)
newtype TaskPoints = TaskPoints Rational deriving (Eq, Show)

-- | Jumped the gun by this many seconds.
newtype JumpedTheGun = JumpedTheGun Rational deriving (Eq, Show)

-- | For this many seconds, loose 1 point.
newtype SecondsPerPoint = SecondsPerPoint Rational deriving (Eq, Show)

-- | A jump of this many seconds incurs the maximum penalty, the score for
-- minimum distance.
newtype JumpTheGunLimit = JumpTheGunLimit Rational deriving (Eq, Show)

newtype NoGoal = NoGoal Bool deriving (Eq, Show)

data Hg = Hg deriving (Show)
data Pg = Pg deriving (Show)

data Penalty a where
    JumpedTooEarly :: MinimumDistancePoints -> Penalty Hg
    Jumped :: SecondsPerPoint -> JumpedTheGun -> Penalty Hg
    JumpedNoGoal :: SecondsPerPoint -> JumpedTheGun -> Penalty Hg
    NoGoalHg :: Penalty Hg
    Early :: LaunchToSssPoints -> Penalty Pg
    NoGoalPg :: Penalty Pg

data PointPenalty
    = PenaltyPoints Integer
    | PenaltyFraction Rational
    deriving (Show)

deriving instance Eq (Penalty a)
deriving instance Show (Penalty a)

data TaskPointParts =
    TaskPointParts
        { distance :: Rational
        , leading :: Rational
        , time :: Rational
        , arrival :: Rational
        } deriving (Eq, Show)

type TaskPointTally = TaskPointParts -> TaskPoints

zeroPoints :: TaskPointParts
zeroPoints = TaskPointParts { distance = 0, leading = 0, time = 0, arrival = 0 }

tallyPoints :: forall a. Maybe (Penalty a) -> TaskPointTally

tallyPoints Nothing =
    \ TaskPointParts {..} -> TaskPoints $ distance + leading + time + arrival

tallyPoints (Just (JumpedTooEarly (MinimumDistancePoints p))) =
    const $ TaskPoints p

tallyPoints (Just (JumpedNoGoal secs jump)) =
    \ TaskPointParts {..} ->
        jumpTheGun secs jump $ TaskPoints $ distance + leading + (8 % 10) * (time + arrival)

tallyPoints (Just (Jumped secs jump)) =
    \ TaskPointParts {..} ->
        jumpTheGun secs jump $ TaskPoints $ distance + leading + time + arrival

tallyPoints (Just NoGoalHg) =
    \ TaskPointParts {..} ->
        TaskPoints $ distance + leading + (8 % 10) * (time + arrival)

tallyPoints (Just (Early (LaunchToSssPoints d))) =
    const $ TaskPoints d

tallyPoints (Just NoGoalPg) =
    \ TaskPointParts {..} -> TaskPoints $ distance + leading

jumpTheGun :: SecondsPerPoint -> JumpedTheGun -> TaskPoints -> TaskPoints
jumpTheGun (SecondsPerPoint secs) (JumpedTheGun jump) (TaskPoints pts) =
    TaskPoints $ max 0 (pts - jump / secs)

taskPoints :: forall a. Maybe (Penalty a) -> TaskPointParts -> TaskPoints
taskPoints = tallyPoints

applyPointPenalty :: PointPenalty-> TaskPoints -> TaskPoints
applyPointPenalty (PenaltyPoints n) (TaskPoints p) =
    TaskPoints $ p - (n % 1)
applyPointPenalty (PenaltyFraction n) (TaskPoints p) =
    TaskPoints $ p - p * n
