module Flight.Gap.Validity.Stop (StopValidity(..)) where

import Data.Typeable (Typeable, typeOf)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific
    ( DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci, deriveShowViaSci)

newtype StopValidity = StopValidity Rational
    deriving (Eq, Ord, Typeable)

instance Newtype StopValidity Rational where
    pack = StopValidity
    unpack (StopValidity a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''StopValidity
deriveJsonViaSci ''StopValidity
deriveShowViaSci ''StopValidity
