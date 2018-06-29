module Flight.Gap.Points.Distance
    ( DistancePoints(..)
    , LinearPoints(..)
    , DifficultyPoints(..)
    ) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype DistancePoints = DistancePoints Rational
    deriving (Eq, Ord, Show)

instance Newtype DistancePoints Rational where
    pack = DistancePoints
    unpack (DistancePoints a) = a

deriveDecimalPlaces (DecimalPlaces 1) ''DistancePoints
deriveJsonViaSci ''DistancePoints

newtype LinearPoints = LinearPoints Rational
    deriving (Eq, Ord, Show)

instance Newtype LinearPoints Rational where
    pack = LinearPoints
    unpack (LinearPoints a) = a

deriveDecimalPlaces (DecimalPlaces 1) ''LinearPoints
deriveJsonViaSci ''LinearPoints

newtype DifficultyPoints = DifficultyPoints Rational
    deriving (Eq, Ord, Show)

instance Newtype DifficultyPoints Rational where
    pack = DifficultyPoints
    unpack (DifficultyPoints a) = a

deriveDecimalPlaces (DecimalPlaces 1) ''DifficultyPoints
deriveJsonViaSci ''DifficultyPoints
