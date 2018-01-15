{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Ratio.Speed (SpeedFraction(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefDec, deriveViaSci)

newtype SpeedFraction = SpeedFraction Rational
    deriving (Eq, Ord, Show)

instance Newtype SpeedFraction Rational where
    pack = SpeedFraction
    unpack (SpeedFraction a) = a

deriveDefDec 8 ''SpeedFraction
deriveViaSci ''SpeedFraction
