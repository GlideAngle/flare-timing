module Flight.Gap.Points.Task (TaskPoints(..)) where

import Text.Printf (printf)
import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype TaskPoints = TaskPoints Double
    deriving stock (Eq, Ord, Generic)
    deriving newtype (Num, Fractional)

instance Show TaskPoints where
    show (TaskPoints x) = printf "TaskPoints %.3f" x

instance Newtype TaskPoints Double where
    pack = TaskPoints
    unpack (TaskPoints a) = a

deriveDecimalPlaces (DecimalPlaces 3) ''TaskPoints
deriveJsonViaSci ''TaskPoints
