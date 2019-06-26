module Flight.Gap.Fraction.Leading
    ( LeadingFraction(..)
    , EssTime(..)
    , LwScaling(..)
    ) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific
    ( DecimalPlaces(..)
    , deriveDecimalPlaces, deriveJsonViaSci, deriveCsvViaSci
    )

newtype LeadingFraction = LeadingFraction Rational
    deriving (Eq, Ord, Show)

instance Newtype LeadingFraction Rational where
    pack = LeadingFraction
    unpack (LeadingFraction a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''LeadingFraction
deriveJsonViaSci ''LeadingFraction
deriveCsvViaSci ''LeadingFraction

-- | Task time when the last pilot made the end of the speed section.
newtype EssTime = EssTime Rational
    deriving (Eq, Ord, Show)

instance Newtype EssTime Rational where
    pack = EssTime
    unpack (EssTime a) = a

deriveDecimalPlaces (DecimalPlaces 3) ''EssTime
deriveJsonViaSci ''EssTime
deriveCsvViaSci ''EssTime

newtype LwScaling = LwScaling Rational
    deriving (Eq, Ord, Show)

instance Newtype LwScaling Rational where
    pack = LwScaling
    unpack (LwScaling a) = a

deriveDecimalPlaces (DecimalPlaces 0) ''LwScaling
deriveJsonViaSci ''LwScaling
deriveCsvViaSci ''LwScaling
