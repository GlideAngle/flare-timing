module Flight.Gap.Fraction.Speed (SpeedFraction(..)) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype SpeedFraction = SpeedFraction Rational
    deriving (Eq, Ord, Show, Generic)

instance Newtype SpeedFraction Rational where
    pack = SpeedFraction
    unpack (SpeedFraction a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''SpeedFraction
deriveJsonViaSci ''SpeedFraction
