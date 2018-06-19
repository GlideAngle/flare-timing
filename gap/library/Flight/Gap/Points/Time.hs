{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Points.Time (TimePoints(..)) where

import Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype TimePoints = TimePoints Rational
    deriving (Eq, Ord, Show)

instance Newtype TimePoints Rational where
    pack = TimePoints
    unpack (TimePoints a) = a

deriveDecimalPlaces (DecimalPlaces 1) ''TimePoints
deriveJsonViaSci ''TimePoints
