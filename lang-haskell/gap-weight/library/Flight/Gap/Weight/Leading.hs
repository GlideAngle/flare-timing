module Flight.Gap.Weight.Leading
    ( LeadingWeight(..)
    , LwScaling(..)
    ) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific
    ( DecimalPlaces(..)
    , deriveDecimalPlaces, deriveJsonViaSci, deriveCsvViaSci
    )

newtype LeadingWeight = LeadingWeight Rational
    deriving (Eq, Ord, Show, Generic)

instance Newtype LeadingWeight Rational where
    pack = LeadingWeight
    unpack (LeadingWeight a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''LeadingWeight
deriveJsonViaSci ''LeadingWeight

newtype LwScaling = LwScaling Rational
    deriving (Eq, Ord, Show, Generic)

instance Newtype LwScaling Rational where
    pack = LwScaling
    unpack (LwScaling a) = a

deriveDecimalPlaces (DecimalPlaces 0) ''LwScaling
deriveJsonViaSci ''LwScaling
deriveCsvViaSci ''LwScaling
