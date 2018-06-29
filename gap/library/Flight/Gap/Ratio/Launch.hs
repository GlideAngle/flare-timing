{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Ratio.Launch (NominalLaunch(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype NominalLaunch = NominalLaunch Rational
    deriving (Eq, Ord, Show)

instance Newtype NominalLaunch Rational where
    pack = NominalLaunch
    unpack (NominalLaunch a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''NominalLaunch
deriveJsonViaSci ''NominalLaunch
