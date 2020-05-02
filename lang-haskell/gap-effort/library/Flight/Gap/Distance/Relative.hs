module Flight.Gap.Distance.Relative (RelativeDifficulty(..)) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

-- | The relative difficulty of a chunk.
newtype RelativeDifficulty = RelativeDifficulty Rational
    deriving (Eq, Ord, Generic)

instance Show RelativeDifficulty where
    show (RelativeDifficulty x) = show (fromRational x :: Double)

instance Newtype RelativeDifficulty Rational where
    pack = RelativeDifficulty
    unpack (RelativeDifficulty a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''RelativeDifficulty
deriveJsonViaSci ''RelativeDifficulty
