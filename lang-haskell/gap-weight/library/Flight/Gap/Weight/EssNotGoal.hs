module Flight.Gap.Weight.EssNotGoal (EGwScaling(..)) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific
    ( DecimalPlaces(..)
    , deriveDecimalPlaces, deriveJsonViaSci, deriveCsvViaSci
    )

-- | The scaling of time and arrival points if ESS is made but not goal.
newtype EGwScaling = EGwScaling Rational
    deriving (Eq, Ord, Show, Generic)

instance Newtype EGwScaling Rational where
    pack = EGwScaling
    unpack (EGwScaling a) = a

deriveDecimalPlaces (DecimalPlaces 3) ''EGwScaling
deriveJsonViaSci ''EGwScaling
deriveCsvViaSci ''EGwScaling
