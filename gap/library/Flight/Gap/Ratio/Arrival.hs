module Flight.Gap.Ratio.Arrival
    ( ArrivalFraction(..)
    , AwScaling(..)
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

newtype AwScaling = AwScaling Rational
    deriving (Eq, Ord, Show)

instance Newtype AwScaling Rational where
    pack = AwScaling
    unpack (AwScaling a) = a

deriveDecimalPlaces (DecimalPlaces 0) ''AwScaling
deriveJsonViaSci ''AwScaling
