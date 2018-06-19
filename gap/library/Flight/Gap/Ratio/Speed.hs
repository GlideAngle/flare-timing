﻿{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Ratio.Speed (SpeedFraction(..)) where

import Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype SpeedFraction = SpeedFraction Rational
    deriving (Eq, Ord, Show)

instance Newtype SpeedFraction Rational where
    pack = SpeedFraction
    unpack (SpeedFraction a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''SpeedFraction
deriveJsonViaSci ''SpeedFraction
