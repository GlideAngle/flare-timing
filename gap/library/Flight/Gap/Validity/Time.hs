module Flight.Gap.Validity.Time (TimeValidity(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype TimeValidity = TimeValidity Rational
    deriving (Eq, Ord, Show)

instance Newtype TimeValidity Rational where
    pack = TimeValidity
    unpack (TimeValidity a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''TimeValidity
deriveJsonViaSci ''TimeValidity
