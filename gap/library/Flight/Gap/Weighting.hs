{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Flight.Gap.Weighting
    ( Weights(..)
    , Lw(..)
    , Aw(..)
    , DistanceRatio(..)
    , distanceWeight
    , leadingWeight
    , arrivalWeight
    , timeWeight
    ) where

import Data.Ratio ((%))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Gap.Ratio (pattern (:%))
import Flight.Gap.Weight.GoalRatio (GoalRatio(..))
import Flight.Gap.Weight.Distance (DistanceWeight(..))
import Flight.Gap.Weight.Leading (LeadingWeight(..))
import Flight.Gap.Weight.Arrival (ArrivalWeight(..))
import Flight.Gap.Weight.Time (TimeWeight(..))

data Weights =
    Weights
        { distance :: DistanceWeight
        , leading :: LeadingWeight
        , arrival :: ArrivalWeight
        , time :: TimeWeight
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | Best distance versus task distance.
newtype DistanceRatio = DistanceRatio Rational deriving (Eq, Show)

data Lw a where
    LwHg  :: DistanceWeight -> Lw Rational
    LwPgZ :: DistanceRatio -> Lw Rational
    LwPg  :: DistanceWeight -> Lw Rational

-- SEE: http://stackoverflow.com/questions/21505975/write-gadt-record-with-constrained-type
deriving instance Show (Lw a)

data Aw a where
    AwHg :: DistanceWeight -> Aw Rational
    AwPg :: Aw ()
deriving instance Show (Aw a)

distanceWeight :: GoalRatio -> DistanceWeight
distanceWeight (GoalRatio gr) =
    DistanceWeight $
    (9 % 10)
    - (1665 % 1000) * gr
    + (1713 % 1000) * gr * gr
    - (587 % 1000) * gr * gr * gr

leadingWeight :: forall a. Lw a -> LeadingWeight
leadingWeight (LwHg (DistanceWeight (n :% d))) =
    LeadingWeight $ ((d - n) % (8 * d)) * (14 % 10)
leadingWeight (LwPgZ (DistanceRatio dr)) =
    LeadingWeight $ dr * (1 % 10)
leadingWeight (LwPg (DistanceWeight (n :% d))) =
    LeadingWeight $ ((d - n) % (8 * d)) * (14 % 10) * (2 % 1)

arrivalWeight :: forall a. Aw a -> ArrivalWeight
arrivalWeight AwPg =
    ArrivalWeight 0
arrivalWeight (AwHg (DistanceWeight (n :% d))) =
    ArrivalWeight $ (d - n) % (8 * d)

timeWeight :: DistanceWeight -> LeadingWeight -> ArrivalWeight -> TimeWeight
timeWeight (DistanceWeight d) (LeadingWeight l) (ArrivalWeight a) =
    TimeWeight $ (1 % 1) - d - l - a
