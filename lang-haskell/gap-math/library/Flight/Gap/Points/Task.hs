module Flight.Gap.Points.Task (TaskPoints(..), onTaskPoints) where

import Text.Printf (printf)
import Test.QuickCheck (Arbitrary(..), Gen)
import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype TaskPoints = TaskPoints Double
    deriving stock (Eq, Ord, Generic)
    deriving newtype (Num, Fractional)

instance Show TaskPoints where
    show (TaskPoints x) = printf "%.3f" x

instance Newtype TaskPoints Double where
    pack = TaskPoints
    unpack (TaskPoints a) = a

instance Arbitrary TaskPoints where
    arbitrary = TaskPoints <$> (arbitrary :: Gen Double)

deriveDecimalPlaces (DecimalPlaces 3) ''TaskPoints
deriveJsonViaSci ''TaskPoints

onTaskPoints :: (Double -> Double) -> TaskPoints -> TaskPoints
onTaskPoints f (TaskPoints x) = TaskPoints $ f x
