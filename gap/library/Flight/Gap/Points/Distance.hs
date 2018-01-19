{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Points.Distance (DistancePoints(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefDec, deriveViaSci)

newtype DistancePoints = DistancePoints Rational
    deriving (Eq, Ord, Show)

instance Newtype DistancePoints Rational where
    pack = DistancePoints
    unpack (DistancePoints a) = a

deriveDefDec 0 ''DistancePoints
deriveViaSci ''DistancePoints
