{-# lANGUAGE PatternSynonyms #-}
{-# lANGUAGE GADTs #-}
{-# lANGUAGE RankNTypes #-}
module Flight.Weighting where

import Data.Ratio ((%))
import Flight.Ratio (pattern (:%))

-- | Pilots in goal versus pilots flying.
type GoalRatio = Rational

-- | Best distance versus task distance.
type DistanceRatio = Rational

type DistanceWeight = Rational
type LeadingWeight = Rational
type ArrivalWeight = Rational
type TimeWeight = Rational

data HgPg = Hg | Pg deriving (Eq, Show)

data Lw a where
    LwHg  :: DistanceWeight -> Lw Rational
    LwPgZ :: DistanceRatio -> Lw Rational
    LwPg  :: DistanceWeight -> Lw Rational

data Aw a where
    AwHg :: DistanceWeight -> Aw Rational
    AwPg :: Aw ()

distanceWeight :: GoalRatio -> DistanceWeight
distanceWeight gr =
    (9 % 10)
    - (1665 % 1000) * gr
    + (1713 % 1000) * gr * gr
    - (587 % 1000) * gr * gr * gr

leadingWeight :: forall a. Lw a -> LeadingWeight
leadingWeight (LwHg (n :% d)) =
    ((d - n) % (8 * d)) * (14 % 10)
leadingWeight (LwPgZ dr) =
    dr * (1 % 10)
leadingWeight (LwPg (n :% d)) =
    ((d - n) % (8 * d)) * (14 % 10) * (2 % 1)

arrivalWeight :: forall a. Aw a -> ArrivalWeight
arrivalWeight AwPg = 0
arrivalWeight (AwHg (n :% d)) =
    (d - n) % (8 * d)

timeWeight :: DistanceWeight -> LeadingWeight -> ArrivalWeight -> TimeWeight
timeWeight d l a = (1 % 1) - d - l - a
