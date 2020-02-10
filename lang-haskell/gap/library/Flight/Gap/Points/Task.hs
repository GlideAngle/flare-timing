module Flight.Gap.Points.Task (TaskPoints(..)) where

import Text.Printf (printf)
import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype TaskPoints = TaskPoints Rational
    deriving (Eq, Ord, Generic)

instance Show TaskPoints where
    show (TaskPoints x) = printf "TaskPoints %.3f" y
        where
            y :: Double
            y = fromRational x

instance Newtype TaskPoints Rational where
    pack = TaskPoints
    unpack (TaskPoints a) = a

deriveDecimalPlaces (DecimalPlaces 3) ''TaskPoints
deriveJsonViaSci ''TaskPoints
