module Flight.Gap.Distance.Relative (RelativeDifficulty(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

-- | The relative difficulty of a chunk.
newtype RelativeDifficulty = RelativeDifficulty Rational
    deriving (Eq, Ord, Show)

instance Newtype RelativeDifficulty Rational where
    pack = RelativeDifficulty
    unpack (RelativeDifficulty a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''RelativeDifficulty
deriveJsonViaSci ''RelativeDifficulty
