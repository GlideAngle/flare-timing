module Flight.Gap.Weight.Distance (DistanceWeight(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype DistanceWeight = DistanceWeight Rational
    deriving (Eq, Ord, Show)

instance Newtype DistanceWeight Rational where
    pack = DistanceWeight
    unpack (DistanceWeight a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''DistanceWeight
deriveJsonViaSci ''DistanceWeight
