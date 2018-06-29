module Flight.Gap.Weight.Leading (LeadingWeight(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype LeadingWeight = LeadingWeight Rational
    deriving (Eq, Ord, Show)

instance Newtype LeadingWeight Rational where
    pack = LeadingWeight
    unpack (LeadingWeight a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''LeadingWeight
deriveJsonViaSci ''LeadingWeight
