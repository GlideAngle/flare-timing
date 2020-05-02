module Flight.Gap.Points.Time (TimePoints(..)) where

import Text.Printf (printf)
import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype TimePoints = TimePoints Rational
    deriving (Eq, Ord, Generic)

instance Show TimePoints where
    show (TimePoints x) = printf "TimePoints %.2f" y
        where
            y :: Double
            y = fromRational x

instance Newtype TimePoints Rational where
    pack = TimePoints
    unpack (TimePoints a) = a

deriveDecimalPlaces (DecimalPlaces 1) ''TimePoints
deriveJsonViaSci ''TimePoints
