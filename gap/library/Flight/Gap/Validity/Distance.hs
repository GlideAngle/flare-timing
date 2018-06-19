{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Validity.Distance (DistanceValidity(..)) where

import Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype DistanceValidity = DistanceValidity Rational
    deriving (Eq, Ord, Show)

instance Newtype DistanceValidity Rational where
    pack = DistanceValidity
    unpack (DistanceValidity a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''DistanceValidity
deriveJsonViaSci ''DistanceValidity
