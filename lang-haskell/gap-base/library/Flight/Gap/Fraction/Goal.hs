module Flight.Gap.Fraction.Goal (NominalGoal(..)) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific
    ( DecimalPlaces(..)
    , deriveDecimalPlaces, deriveJsonViaSci, deriveShowValueViaSci
    )

newtype NominalGoal = NominalGoal Rational
    deriving (Eq, Ord, Generic)

instance Newtype NominalGoal Rational where
    pack = NominalGoal
    unpack (NominalGoal a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''NominalGoal
deriveJsonViaSci ''NominalGoal
deriveShowValueViaSci ''NominalGoal
