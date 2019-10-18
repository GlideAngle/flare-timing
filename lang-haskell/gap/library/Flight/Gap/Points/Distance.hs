module Flight.Gap.Points.Distance
    ( DistancePoints(..)
    , LinearPoints(..)
    , DifficultyPoints(..)
    ) where

import Text.Printf (printf)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype DistancePoints = DistancePoints Rational
    deriving (Eq, Ord)

instance Show DistancePoints where
    show (DistancePoints x) = printf "DistancePoints %.2f" y
        where
            y :: Double
            y = fromRational x

instance Newtype DistancePoints Rational where
    pack = DistancePoints
    unpack (DistancePoints a) = a

deriveDecimalPlaces (DecimalPlaces 3) ''DistancePoints
deriveJsonViaSci ''DistancePoints

newtype LinearPoints = LinearPoints Rational
    deriving (Eq, Ord)

instance Show LinearPoints where
    show (LinearPoints x) = printf "LinearPoints %.2f" y
        where
            y :: Double
            y = fromRational x

instance Newtype LinearPoints Rational where
    pack = LinearPoints
    unpack (LinearPoints a) = a

deriveDecimalPlaces (DecimalPlaces 3) ''LinearPoints
deriveJsonViaSci ''LinearPoints

newtype DifficultyPoints = DifficultyPoints Rational
    deriving (Eq, Ord)

instance Show DifficultyPoints where
    show (DifficultyPoints x) = printf "DifficultyPoints %.2f" y
        where
            y :: Double
            y = fromRational x

instance Newtype DifficultyPoints Rational where
    pack = DifficultyPoints
    unpack (DifficultyPoints a) = a

deriveDecimalPlaces (DecimalPlaces 3) ''DifficultyPoints
deriveJsonViaSci ''DifficultyPoints
