{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Ratio.Launch (NominalLaunch(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefaultDecimalPlaces, deriveViaSci)

newtype NominalLaunch = NominalLaunch Rational
    deriving (Eq, Ord, Show)

instance Newtype NominalLaunch Rational where
    pack = NominalLaunch
    unpack (NominalLaunch a) = a

deriveDefaultDecimalPlaces 8 ''NominalLaunch
deriveViaSci ''NominalLaunch
