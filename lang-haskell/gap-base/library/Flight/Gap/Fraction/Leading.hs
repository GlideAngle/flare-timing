module Flight.Gap.Fraction.Leading (LeadingFraction(..)) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific
    ( DecimalPlaces(..)
    , deriveDecimalPlaces, deriveJsonViaSci, deriveCsvViaSci
    )

newtype LeadingFraction = LeadingFraction Rational
    deriving (Eq, Ord, Show, Generic)

instance Newtype LeadingFraction Rational where
    pack = LeadingFraction
    unpack (LeadingFraction a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''LeadingFraction
deriveJsonViaSci ''LeadingFraction
deriveCsvViaSci ''LeadingFraction
