module Flight.Gap.Weight.Distance
    ( DistanceWeight(..)
    , ReachWeight(..)
    , EffortWeight(..)
    ) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype DistanceWeight = DistanceWeight Rational
    deriving (Eq, Ord, Show, Generic)

newtype ReachWeight = ReachWeight Rational
    deriving (Eq, Ord, Show, Generic)

newtype EffortWeight = EffortWeight Rational
    deriving (Eq, Ord, Show, Generic)

instance Newtype DistanceWeight Rational where
    pack = DistanceWeight
    unpack (DistanceWeight a) = a

instance Newtype ReachWeight Rational where
    pack = ReachWeight
    unpack (ReachWeight a) = a

instance Newtype EffortWeight Rational where
    pack = EffortWeight
    unpack (EffortWeight a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''DistanceWeight
deriveDecimalPlaces (DecimalPlaces 8) ''ReachWeight
deriveDecimalPlaces (DecimalPlaces 8) ''EffortWeight

deriveJsonViaSci ''DistanceWeight
deriveJsonViaSci ''ReachWeight
deriveJsonViaSci ''EffortWeight
