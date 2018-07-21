module Flight.Gap.Ratio.Launch (NominalLaunch(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

{-@ newtype NominalLaunch = NominalLaunch {x :: Rational } @-}
newtype NominalLaunch = NominalLaunch Rational
    deriving (Eq, Ord, Read, Show)

instance Newtype NominalLaunch Rational where
    pack = NominalLaunch
    unpack (NominalLaunch a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''NominalLaunch
deriveJsonViaSci ''NominalLaunch
