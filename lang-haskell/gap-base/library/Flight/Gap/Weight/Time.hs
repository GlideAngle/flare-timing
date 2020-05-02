module Flight.Gap.Weight.Time (TimeWeight(..)) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype TimeWeight = TimeWeight Rational
    deriving (Eq, Ord, Show, Generic)

instance Newtype TimeWeight Rational where
    pack = TimeWeight
    unpack (TimeWeight a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''TimeWeight
deriveJsonViaSci ''TimeWeight
