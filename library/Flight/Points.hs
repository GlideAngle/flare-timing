module Flight.Points where

import Data.Ratio ((%))

newtype LaunchToSssPoints = LaunchToSssPoints Rational deriving (Eq, Show)
newtype MinimumDistancePoints = MinimumDistancePoints Rational deriving (Eq, Show)
newtype DistancePoints = DistancePoints Rational deriving (Eq, Show)
newtype LeadingPoints = LeadingPoints Rational deriving (Eq, Show)
newtype TimePoints = TimePoints Rational deriving (Eq, Show)
newtype ArrivalPoints = ArrivalPoints Rational deriving (Eq, Show)
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
    | EarlyStartNot
    deriving Show

data EssButNotGoalPenalty
    = EssButNotGoalPg
    | EssButNotGoalHg
    | EssButNotGoalNot
    deriving Show

jumpTheGun :: SecondsPerPoint -> JumpedTheGun -> TaskPoints -> TaskPoints
jumpTheGun (SecondsPerPoint secs) (JumpedTheGun jump) (TaskPoints pts) =
    TaskPoints $ pts - jump / secs

taskPoints :: EssButNotGoalPenalty
              -> EarlyStartPenalty
              -> DistancePoints
              -> LeadingPoints
              -> TimePoints
              -> ArrivalPoints
              -> TaskPoints
taskPoints _ (EarlyStartHgMax (MinimumDistancePoints p)) _ _ _ _ =
    TaskPoints p
taskPoints
    EssButNotGoalHg
    (EarlyStartHg secs jump)
    (DistancePoints d)
    (LeadingPoints l)
    (TimePoints t)
    (ArrivalPoints a) =
    jumpTheGun secs jump $ TaskPoints $ d + l + (8 % 10) * (t + a)
taskPoints
    _
    (EarlyStartHg secs jump)
    (DistancePoints d)
    (LeadingPoints l)
    (TimePoints t)
    (ArrivalPoints a) =
    jumpTheGun secs jump $ TaskPoints $ d + l + t + a
taskPoints
    EssButNotGoalHg
    _
    (DistancePoints d)
    (LeadingPoints l)
    (TimePoints t)
    (ArrivalPoints a) =
    TaskPoints $ d + l + (8 % 10) * (t + a)
taskPoints
    _
    (EarlyStartPg (LaunchToSssPoints d))
    _
    _
    _
    _ =
    TaskPoints d
taskPoints
    EssButNotGoalPg
    _
    (DistancePoints d)
    (LeadingPoints l)
    _
    _ =
    TaskPoints $ d + l
taskPoints
    _
    _
    (DistancePoints d)
    (LeadingPoints l)
    (TimePoints t)
    (ArrivalPoints a) =
    TaskPoints $ d + l + t + a
