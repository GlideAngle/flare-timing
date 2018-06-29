module Flight.Gap.Points.Arrival (ArrivalPoints(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype ArrivalPoints = ArrivalPoints Rational
    deriving (Eq, Ord, Show)

instance Newtype ArrivalPoints Rational where
    pack = ArrivalPoints
    unpack (ArrivalPoints a) = a

deriveDecimalPlaces (DecimalPlaces 1) ''ArrivalPoints
deriveJsonViaSci ''ArrivalPoints
