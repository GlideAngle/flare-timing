module Flight.Gap.Equation
    ( PowerExponent(..)
    , powerExp23, powerExp56
    , powerFraction
    , arrivalTimePowerFraction
    ) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Gap.Time.Arrival (ArrivalLag(..))
import Flight.Gap.Fraction.Arrival (ArrivalFraction(..))

newtype PowerExponent = PowerExponent Double
    deriving (Eq, Ord, Show, Generic)

instance Newtype PowerExponent Double where
    pack = PowerExponent
    unpack (PowerExponent a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''PowerExponent
deriveJsonViaSci ''PowerExponent

powerExp23, powerExp56 :: PowerExponent
powerExp23 = PowerExponent $ 2/3
powerExp56 = PowerExponent $ 5/6

-- |
-- prop> powerFraction 0 x == 0
-- prop> powerFraction x 0 == 0
-- prop> powerFraction x y >= 0
-- prop> powerFraction x y <= 1
-- False
powerFraction :: PowerExponent -> Double -> Double -> Double
powerFraction _ 0 _ = 0
powerFraction _ _ 0 = 0
powerFraction (PowerExponent pe) c x =
    max 0 $ 1 - frac
    where
        numerator = x - c
        denominator = c ** (1/2)
        frac = (numerator / denominator) ** pe

arrivalTimePowerFraction
    :: ArrivalLag (Quantity Double [u| h |])
    -> ArrivalFraction
arrivalTimePowerFraction (ArrivalLag (MkQuantity lag)) =
    ArrivalFraction . toRational . max 0 $ (1 - (2.0/3.0) * lag) ** 3
