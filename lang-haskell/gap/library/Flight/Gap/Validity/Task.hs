module Flight.Gap.Validity.Task (TaskValidity(..)) where

import GHC.Generics (Generic)
import Data.Typeable (Typeable, typeOf)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific
    ( DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci, deriveShowViaSci)

-- | Also called Day Quality.
newtype TaskValidity = TaskValidity Rational
    deriving (Eq, Ord, Typeable, Generic)

instance Newtype TaskValidity Rational where
    pack = TaskValidity
    unpack (TaskValidity a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''TaskValidity
deriveJsonViaSci ''TaskValidity
deriveShowViaSci ''TaskValidity
