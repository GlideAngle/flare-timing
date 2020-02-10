module Flight.Gap.Points.Leading (LeadingPoints(..)) where

import Text.Printf (printf)
import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype LeadingPoints = LeadingPoints Rational
    deriving (Eq, Ord, Generic)

instance Show LeadingPoints where
    show (LeadingPoints x) = printf "LeadingPoints %.2f" y
        where
            y :: Double
            y = fromRational x

instance Newtype LeadingPoints Rational where
    pack = LeadingPoints
    unpack (LeadingPoints a) = a

deriveDecimalPlaces (DecimalPlaces 1) ''LeadingPoints
deriveJsonViaSci ''LeadingPoints
