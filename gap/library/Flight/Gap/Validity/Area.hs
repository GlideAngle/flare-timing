{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Validity.Area (NominalDistanceArea(..)) where

import Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype NominalDistanceArea = NominalDistanceArea Rational
    deriving (Eq, Ord, Show)

instance Newtype NominalDistanceArea Rational where
    pack = NominalDistanceArea
    unpack (NominalDistanceArea a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''NominalDistanceArea
deriveJsonViaSci ''NominalDistanceArea
