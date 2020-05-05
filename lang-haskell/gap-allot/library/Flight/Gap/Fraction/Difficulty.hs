module Flight.Gap.Fraction.Difficulty (DifficultyFraction(..)) where

import Text.Printf (printf)
import qualified Data.CReal as ExactReal
import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)
import Data.Ratio.Rounding (sdRound)

type CReal = ExactReal.CReal 12

roundCReal :: CReal -> Double
roundCReal = fromRational . sdRound 3 . toRational

-- | The sum of relative difficulties up until the chunk of landing.
newtype DifficultyFraction = DifficultyFraction CReal
    deriving (Eq, Ord, Generic)

instance Show DifficultyFraction where
    show (DifficultyFraction d) = printf "%f" $ roundCReal d

instance Newtype DifficultyFraction CReal where
    pack = DifficultyFraction
    unpack (DifficultyFraction a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''DifficultyFraction
deriveJsonViaSci ''DifficultyFraction
