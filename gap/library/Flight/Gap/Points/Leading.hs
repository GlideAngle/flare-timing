﻿{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Points.Leading (LeadingPoints(..)) where

import Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype LeadingPoints = LeadingPoints Rational
    deriving (Eq, Ord, Show)

instance Newtype LeadingPoints Rational where
    pack = LeadingPoints
    unpack (LeadingPoints a) = a

deriveDecimalPlaces (DecimalPlaces 1) ''LeadingPoints
deriveJsonViaSci ''LeadingPoints
