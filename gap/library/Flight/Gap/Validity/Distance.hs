{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Validity.Distance (DistanceValidity(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefaultDecimalPlaces, deriveViaSci)

newtype DistanceValidity = DistanceValidity Rational
    deriving (Eq, Ord, Show)

instance Newtype DistanceValidity Rational where
    pack = DistanceValidity
    unpack (DistanceValidity a) = a

deriveDefaultDecimalPlaces 8 ''DistanceValidity
deriveViaSci ''DistanceValidity
