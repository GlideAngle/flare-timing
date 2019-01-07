module Flight.Gap.Weighting
    ( Weights(..)
    , Lw(..)
    , Aw(..)
    , Rw(..)
    , Ew(..)
    , DistanceRatio(..)
    , distanceWeight
    , reachWeight
    , effortWeight
    , leadingWeight
    , arrivalWeight
    , timeWeight
    ) where

import Data.Ratio ((%))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Ratio (pattern (:%))
import Flight.Gap.Weight.GoalRatio (GoalRatio(..))
import Flight.Gap.Weight.Distance
    (DistanceWeight(..), ReachWeight(..), EffortWeight(..))
import Flight.Gap.Weight.Leading (LeadingWeight(..))
import Flight.Gap.Weight.Arrival (ArrivalWeight(..))
import Flight.Gap.Weight.Time (TimeWeight(..))

data Weights =
    Weights
        { distance :: DistanceWeight
        , reach :: ReachWeight
        , effort :: EffortWeight
        , leading :: LeadingWeight
        , arrival :: ArrivalWeight
        , time :: TimeWeight
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | Best distance versus task distance.
newtype DistanceRatio = DistanceRatio Rational deriving (Eq, Show)

-- | Leading weight varies between disciplines and in paragliding its
-- calculation inputs change depending on whether any pilots make goal or not.
data Lw
    = LwHg DistanceWeight
    | LwPgZ DistanceRatio
    | LwPg DistanceWeight
    deriving Show

-- | Arrival weight is only for hang gliding.
data Aw
    = AwHg DistanceWeight
    | AwPg
    deriving Show

-- | Reach weight varies between disciplines.
data Rw
    = RwHg DistanceWeight
    | RwPg DistanceWeight
    deriving Show

-- | Effort weight is only for hang gliding.
data Ew
    = EwHg DistanceWeight
    | EwPg
    deriving Show

reachWeight :: Rw -> ReachWeight
reachWeight (RwHg (DistanceWeight (n :% d))) = ReachWeight $ n % (d * 2)
reachWeight (RwPg (DistanceWeight w)) = ReachWeight w

effortWeight :: Ew -> EffortWeight
effortWeight (EwHg (DistanceWeight (n :% d))) = EffortWeight $ n % (d * 2)
effortWeight EwPg = EffortWeight 0

distanceWeight :: GoalRatio -> DistanceWeight
distanceWeight (GoalRatio gr) =
    DistanceWeight $
    (9 % 10)
    - (1665 % 1000) * gr
    + (1713 % 1000) * gr * gr
    - (587 % 1000) * gr * gr * gr

leadingWeight :: Lw -> LeadingWeight
leadingWeight (LwHg (DistanceWeight (n :% d))) =
    LeadingWeight $ ((d - n) % (8 * d)) * (14 % 10)
leadingWeight (LwPgZ (DistanceRatio dr)) =
    LeadingWeight $ dr * (1 % 10)
leadingWeight (LwPg (DistanceWeight (n :% d))) =
    LeadingWeight $ ((d - n) % (8 * d)) * (14 % 10) * (2 % 1)

arrivalWeight :: Aw -> ArrivalWeight
arrivalWeight AwPg =
    ArrivalWeight 0
arrivalWeight (AwHg (DistanceWeight (n :% d))) =
    ArrivalWeight $ (d - n) % (8 * d)

timeWeight :: DistanceWeight -> LeadingWeight -> ArrivalWeight -> TimeWeight
timeWeight (DistanceWeight d) (LeadingWeight l) (ArrivalWeight a) =
    TimeWeight $ (1 % 1) - d - l - a
