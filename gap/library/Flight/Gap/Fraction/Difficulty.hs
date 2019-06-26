module Flight.Gap.Fraction.Difficulty (DifficultyFraction(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

-- | The sum of relative difficulties up until the chunk of landing.
newtype DifficultyFraction = DifficultyFraction Rational
    deriving (Eq, Ord, Show)

instance Newtype DifficultyFraction Rational where
    pack = DifficultyFraction
    unpack (DifficultyFraction a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''DifficultyFraction
deriveJsonViaSci ''DifficultyFraction
