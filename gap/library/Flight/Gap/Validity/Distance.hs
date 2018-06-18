{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Validity.Distance (DistanceValidity(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefDec, deriveViaSci)

newtype DistanceValidity = DistanceValidity Rational
    deriving (Eq, Ord, Show)

instance Newtype DistanceValidity Rational where
    pack = DistanceValidity
    unpack (DistanceValidity a) = a

deriveDefDec 8 ''DistanceValidity
deriveViaSci ''DistanceValidity
