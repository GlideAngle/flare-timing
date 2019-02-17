module Flight.Gap.Validity.Distance (DistanceValidity(..)) where

import Data.Typeable (Typeable, typeOf)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific
    (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci, deriveShowViaSci)

newtype DistanceValidity = DistanceValidity Rational
    deriving (Eq, Ord, Typeable)

instance Newtype DistanceValidity Rational where
    pack = DistanceValidity
    unpack (DistanceValidity a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''DistanceValidity
deriveJsonViaSci ''DistanceValidity
deriveShowViaSci ''DistanceValidity
