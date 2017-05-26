{-# LANGUAGE RecordWildCards #-}
module Flight.Points
    ( LaunchToSssPoints(..)
    , MinimumDistancePoints(..)
    , SecondsPerPoint(..)
    , JumpedTheGun(..)
    , EarlyStartPenalty(..)
    , NoGoalPenalty(..)
    , TaskPenalties(..)
    , TaskPointParts(..)
    , TaskPoints(..)
    , zeroPenalties
    , zeroPoints
    , taskPoints
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

data EarlyStartPenalty
    = EarlyStartHgMax MinimumDistancePoints
    | EarlyStartHg SecondsPerPoint JumpedTheGun
    | EarlyStartPg LaunchToSssPoints
    deriving (Eq, Show)

-- | When a pilot completes the speed section but misses goal.
data NoGoalPenalty
    = NoGoalPg
    | NoGoalHg
    deriving (Eq, Show)

data TaskPenalties =
    TaskPenalties
        { earlyStart :: Maybe EarlyStartPenalty
        , noGoal :: Maybe NoGoalPenalty
        } deriving (Eq, Show)

data TaskPointParts =
    TaskPointParts
        { distance :: Rational
        , leading :: Rational
        , time :: Rational
        , arrival :: Rational
        } deriving (Eq, Show)

type TaskPointTally = TaskPointParts -> TaskPoints

zeroPenalties :: TaskPenalties
zeroPenalties = TaskPenalties { earlyStart = Nothing, noGoal = Nothing }

zeroPoints :: TaskPointParts
zeroPoints = TaskPointParts { distance = 0, leading = 0, time = 0, arrival = 0 }

tallyPoints :: Maybe EarlyStartPenalty -> Maybe NoGoalPenalty -> TaskPointTally
tallyPoints (Just (EarlyStartHgMax (MinimumDistancePoints p))) _ =
    const $ TaskPoints p
tallyPoints (Just (EarlyStartHg secs jump)) (Just NoGoalHg) =
    \ TaskPointParts {..} ->
        jumpTheGun secs jump $ TaskPoints $ distance + leading + (8 % 10) * (time + arrival)
tallyPoints (Just (EarlyStartHg secs jump)) _ =
    \ TaskPointParts {..} ->
        jumpTheGun secs jump $ TaskPoints $ distance + leading + time + arrival
tallyPoints _ (Just NoGoalHg) =
    \ TaskPointParts {..} ->
        TaskPoints $ distance + leading + (8 % 10) * (time + arrival)
tallyPoints (Just (EarlyStartPg (LaunchToSssPoints d))) _ =
    const $ TaskPoints d
tallyPoints _ (Just NoGoalPg) =
    \ TaskPointParts {..} ->
        TaskPoints $ distance + leading
tallyPoints _ _ =
    \ TaskPointParts {..} ->
        TaskPoints $ distance + leading + time + arrival

jumpTheGun :: SecondsPerPoint -> JumpedTheGun -> TaskPoints -> TaskPoints
jumpTheGun (SecondsPerPoint secs) (JumpedTheGun jump) (TaskPoints pts) =
    TaskPoints $ max 0 $ pts - jump / secs

taskPoints :: TaskPenalties -> TaskPointParts -> TaskPoints
taskPoints TaskPenalties{..} parts =
    tallyPoints earlyStart noGoal parts
