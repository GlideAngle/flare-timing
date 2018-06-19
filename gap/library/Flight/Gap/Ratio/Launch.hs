{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Ratio.Launch (NominalLaunch(..)) where

import Control.Newtype (Newtype(..))
import Data.Via.Scientific (deriveDefDec, deriveViaSci)

newtype NominalLaunch = NominalLaunch Rational
    deriving (Eq, Ord, Show)

instance Newtype NominalLaunch Rational where
    pack = NominalLaunch
    unpack (NominalLaunch a) = a

deriveDefDec 8 ''NominalLaunch
deriveViaSci ''NominalLaunch
