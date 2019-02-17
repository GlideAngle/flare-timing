module Flight.Gap.Validity.Time (TimeValidity(..)) where

import Data.Typeable (Typeable, typeOf)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific
    (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci, deriveShowViaSci)

newtype TimeValidity = TimeValidity Rational
    deriving (Eq, Ord, Typeable)

instance Newtype TimeValidity Rational where
    pack = TimeValidity
    unpack (TimeValidity a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''TimeValidity
deriveJsonViaSci ''TimeValidity
deriveShowViaSci ''TimeValidity
