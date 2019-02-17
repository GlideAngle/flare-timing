module Flight.Gap.Ratio.Goal (NominalGoal(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific
    ( DecimalPlaces(..)
    , deriveDecimalPlaces, deriveJsonViaSci, deriveShowValueViaSci
    )

newtype NominalGoal = NominalGoal Rational
    deriving (Eq, Ord)

instance Newtype NominalGoal Rational where
    pack = NominalGoal
    unpack (NominalGoal a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''NominalGoal
deriveJsonViaSci ''NominalGoal
deriveShowValueViaSci ''NominalGoal
