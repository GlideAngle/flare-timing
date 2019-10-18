module Flight.Gap.Fraction.Distance (DistanceFraction(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype DistanceFraction = DistanceFraction Rational
    deriving (Eq, Ord, Show)

instance Newtype DistanceFraction Rational where
    pack = DistanceFraction
    unpack (DistanceFraction a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''DistanceFraction
deriveJsonViaSci ''DistanceFraction
