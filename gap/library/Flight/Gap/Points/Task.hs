module Flight.Gap.Points.Task (TaskPoints(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype TaskPoints = TaskPoints Rational
    deriving (Eq, Ord, Show)

instance Newtype TaskPoints Rational where
    pack = TaskPoints
    unpack (TaskPoints a) = a

deriveDecimalPlaces (DecimalPlaces 0) ''TaskPoints
deriveJsonViaSci ''TaskPoints
