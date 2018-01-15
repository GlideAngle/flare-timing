{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Weight.Distance (DistanceWeight(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefaultDecimalPlaces, deriveViaSci)

newtype DistanceWeight = DistanceWeight Rational
    deriving (Eq, Ord, Show)

instance Newtype DistanceWeight Rational where
    pack = DistanceWeight
    unpack (DistanceWeight a) = a

deriveDefaultDecimalPlaces 8 ''DistanceWeight
deriveViaSci ''DistanceWeight
