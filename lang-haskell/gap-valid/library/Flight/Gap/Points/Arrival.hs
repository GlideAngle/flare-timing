module Flight.Gap.Points.Arrival (ArrivalPoints(..)) where

import Text.Printf (printf)
import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype ArrivalPoints = ArrivalPoints Rational
    deriving (Eq, Ord, Generic)

instance Show ArrivalPoints where
    show (ArrivalPoints x) = printf "ArrivalPoints %.2f" y
        where
            y :: Double
            y = fromRational x

instance Newtype ArrivalPoints Rational where
    pack = ArrivalPoints
    unpack (ArrivalPoints a) = a

deriveDecimalPlaces (DecimalPlaces 1) ''ArrivalPoints
deriveJsonViaSci ''ArrivalPoints
