{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- WARNING: This extension needs to be enabled at the definition site of a set
-- of record fields in order for them to be re-exported by a single module.
-- SEE: https://ghc.haskell.org/trac/ghc/ticket/13352
{-# LANGUAGE DuplicateRecordFields #-}

module Flight.Gap.Points
    ( LaunchToSssPoints(..)
    , MinimumDistancePoints(..)
    , SecondsPerPoint(..)
    , JumpedTheGun(..)
    , Hg
    , Pg
    , Penalty(..)
    , Points(..)
    , zeroPoints
    , taskPoints
    , applyPointPenalty
    , availablePoints
    ) where

import Data.Ratio ((%))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Gap.Points.Distance (DistancePoints(..))
import Flight.Gap.Points.Time (TimePoints(..))
import Flight.Gap.Points.Leading (LeadingPoints(..))
import Flight.Gap.Points.Arrival (ArrivalPoints(..))
import Flight.Gap.Points.Task (TaskPoints(..))
import Flight.Gap.Validity.Task (TaskValidity(..))
import Flight.Gap.Weighting (Weights(..))
import Flight.Gap.Weight.Distance (DistanceWeight(..))
import Flight.Gap.Weight.Leading (LeadingWeight(..))
import Flight.Gap.Weight.Arrival (ArrivalWeight(..))
import Flight.Gap.Weight.Time (TimeWeight(..))

newtype LaunchToSssPoints = LaunchToSssPoints Rational deriving (Eq, Show)
newtype MinimumDistancePoints = MinimumDistancePoints Rational deriving (Eq, Show)

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

data Points =
    Points 
        { distance :: DistancePoints
        , leading :: LeadingPoints
        , arrival :: ArrivalPoints
        , time :: TimePoints
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

type TaskPointTally = Points -> TaskPoints

zeroPoints :: Points
zeroPoints =
    Points
        { distance = DistancePoints 0
        , leading = LeadingPoints 0
        , time = TimePoints 0
        , arrival = ArrivalPoints 0
        }

tallyPoints :: forall a. Maybe (Penalty a) -> TaskPointTally

tallyPoints Nothing =
    \Points
        { distance = DistancePoints d
        , leading = LeadingPoints l
        , time = TimePoints t
        , arrival = ArrivalPoints a
        } -> TaskPoints $ d + l + t + a

tallyPoints (Just (JumpedTooEarly (MinimumDistancePoints p))) =
    const $ TaskPoints p

tallyPoints (Just (JumpedNoGoal secs jump)) =
    \Points
        { distance = DistancePoints d
        , leading = LeadingPoints l
        , time = TimePoints t
        , arrival = ArrivalPoints a
        }-> jumpTheGun secs jump . TaskPoints $ d + l + (8 % 10) * (t + a)

tallyPoints (Just (Jumped secs jump)) =
    \Points
        { distance = DistancePoints d
        , leading = LeadingPoints l
        , time = TimePoints t
        , arrival = ArrivalPoints a
        } -> jumpTheGun secs jump . TaskPoints $ d + l + t + a

tallyPoints (Just NoGoalHg) =
    \Points
        { distance = DistancePoints d
        , leading = LeadingPoints l
        , time = TimePoints t
        , arrival = ArrivalPoints a
        } -> TaskPoints $ d + l + (8 % 10) * (t + a)

tallyPoints (Just (Early (LaunchToSssPoints d))) =
    const $ TaskPoints d

tallyPoints (Just NoGoalPg) =
    \Points
        { distance = DistancePoints d
        , leading = LeadingPoints l
        } -> TaskPoints $ d + l

jumpTheGun :: SecondsPerPoint -> JumpedTheGun -> TaskPoints -> TaskPoints
jumpTheGun (SecondsPerPoint secs) (JumpedTheGun jump) (TaskPoints pts) =
    TaskPoints $ max 0 (pts - jump / secs)

taskPoints :: forall a. Maybe (Penalty a) -> Points -> TaskPoints
taskPoints = tallyPoints

applyPointPenalty :: PointPenalty-> TaskPoints -> TaskPoints
applyPointPenalty (PenaltyPoints n) (TaskPoints p) =
    TaskPoints $ p - (n % 1)
applyPointPenalty (PenaltyFraction n) (TaskPoints p) =
    TaskPoints $ p - p * n

availablePoints :: TaskValidity -> Weights -> (Points, TaskPoints)
availablePoints (TaskValidity tv) Weights{..} =
    (pts, tallyPoints Nothing pts)
    where
        DistanceWeight dw = distance
        LeadingWeight lw = leading
        ArrivalWeight aw = arrival
        TimeWeight tw = time
        k x = 1000 * tv * x

        pts =
            Points
                { distance = DistancePoints . k $ dw
                , leading = LeadingPoints . k $ lw
                , arrival = ArrivalPoints . k $ aw
                , time = TimePoints . k $ tw
                }
