module Flight.Gap.Ratio.Leading
    ( LeadingAreaScaling(..)
    , LeadingAreaStep(..)
    , LeadingCoefficient(..)
    , LeadingFraction(..)
    , EssTime(..)
    , LwScaling(..)
    ) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci, deriveCsvViaSci)

newtype LeadingAreaScaling = LeadingAreaScaling Rational
    deriving (Eq, Ord, Show)

instance Newtype LeadingAreaScaling Rational where
    pack = LeadingAreaScaling
    unpack (LeadingAreaScaling a) = a

deriveDecimalPlaces (DecimalPlaces 14) ''LeadingAreaScaling
deriveJsonViaSci ''LeadingAreaScaling
deriveCsvViaSci ''LeadingAreaScaling

newtype LeadingAreaStep = LeadingAreaStep Rational
    deriving (Eq, Ord, Show)

instance Newtype LeadingAreaStep Rational where
    pack = LeadingAreaStep
    unpack (LeadingAreaStep a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''LeadingAreaStep
deriveJsonViaSci ''LeadingAreaStep
deriveCsvViaSci ''LeadingAreaStep

newtype LeadingCoefficient = LeadingCoefficient Rational
    deriving (Eq, Ord, Show)

instance Newtype LeadingCoefficient Rational where
    pack = LeadingCoefficient
    unpack (LeadingCoefficient a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''LeadingCoefficient
deriveJsonViaSci ''LeadingCoefficient
deriveCsvViaSci ''LeadingCoefficient

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
