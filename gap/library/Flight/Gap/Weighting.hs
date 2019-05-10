module Flight.Gap.Weighting
    ( Weights(..)
    , Lw(..)
    , Aw(..)
    , Rw(..)
    , Ew(..)
    , DistanceRatio(..)
    , distanceRatio
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
import Data.UnitsOfMeasure ((/:), u, unQuantity, toRational', zero)
import Data.UnitsOfMeasure.Convert (Convertible)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Ratio (pattern (:%))
import Flight.Gap.Weight.GoalRatio (GoalRatio(..))
import Flight.Gap.Weight.Distance
    (DistanceWeight(..), ReachWeight(..), EffortWeight(..))
import Flight.Gap.Weight.Leading (LeadingWeight(..))
import Flight.Gap.Weight.Arrival (ArrivalWeight(..))
import Flight.Gap.Weight.Time (TimeWeight(..))
import Flight.Gap.Ratio.Arrival (AwScaling(..))
import Flight.Gap.Ratio.Leading (LwScaling(..))

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

distanceRatio
    :: (RealFrac a, Convertible u [u| m |])
    => Quantity a u
    -> Quantity a u
    -> DistanceRatio
distanceRatio bd td =
    if td == zero then DistanceRatio 0 else
    DistanceRatio . unQuantity . toRational' $ bd /: td

-- | Leading weight varies between disciplines and in paragliding its
-- calculation inputs change depending on whether any pilots make goal or not.
data Lw
    = LwHg DistanceWeight
    | LwPgZ DistanceRatio
    | LwPg DistanceWeight
    | LwScaled LwScaling DistanceWeight
    deriving Show

-- | Arrival weight is only for hang gliding.
data Aw
    = AwHg DistanceWeight
    | AwPg
    | AwScaled AwScaling DistanceWeight
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

lwDistanceWeight :: DistanceWeight -> Rational
lwDistanceWeight (DistanceWeight dw) =
    (1 - dw) * (1 % 8) * (14 % 10)

leadingWeight :: Lw -> LeadingWeight
leadingWeight (LwHg dw) =
    LeadingWeight $ lwDistanceWeight dw
leadingWeight (LwPgZ (DistanceRatio dr)) =
    LeadingWeight $ dr * (1 % 10)
leadingWeight (LwPg dw) =
    LeadingWeight $ lwDistanceWeight dw * (2 % 1)
leadingWeight (LwScaled (LwScaling k) dw) =
    LeadingWeight $ lwDistanceWeight dw * k

awDistanceWeight :: DistanceWeight -> Rational
awDistanceWeight (DistanceWeight (n :% d)) =
    (d - n) % (8 * d)

arrivalWeight :: Aw -> ArrivalWeight
arrivalWeight AwPg =
    ArrivalWeight 0
arrivalWeight (AwHg dw) =
    ArrivalWeight $ awDistanceWeight dw
arrivalWeight (AwScaled (AwScaling k) dw) =
    ArrivalWeight $ awDistanceWeight dw * k

timeWeight :: DistanceWeight -> LeadingWeight -> ArrivalWeight -> TimeWeight
timeWeight (DistanceWeight d) (LeadingWeight l) (ArrivalWeight a) =
    TimeWeight $ (1 % 1) - d - l - a
