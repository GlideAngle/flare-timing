module Flight.Gap.Fraction.Arrival
    ( ArrivalFraction(..)
    ) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype ArrivalFraction = ArrivalFraction Rational
    deriving (Eq, Ord, Show)

instance Newtype ArrivalFraction Rational where
    pack = ArrivalFraction
    unpack (ArrivalFraction a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''ArrivalFraction
deriveJsonViaSci ''ArrivalFraction
