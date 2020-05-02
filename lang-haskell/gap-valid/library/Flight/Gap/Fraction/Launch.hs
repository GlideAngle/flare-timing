module Flight.Gap.Fraction.Launch (NominalLaunch(..)) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific
    ( DecimalPlaces(..)
    , deriveDecimalPlaces, deriveJsonViaSci, deriveShowValueViaSci
    )

{-@ newtype NominalLaunch = NominalLaunch {x :: Rational } @-}
newtype NominalLaunch = NominalLaunch Rational
    deriving (Eq, Ord, Read, Generic)

instance Newtype NominalLaunch Rational where
    pack = NominalLaunch
    unpack (NominalLaunch a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''NominalLaunch
deriveJsonViaSci ''NominalLaunch
deriveShowValueViaSci ''NominalLaunch
