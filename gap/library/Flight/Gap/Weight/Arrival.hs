{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Weight.Arrival (ArrivalWeight(..)) where

import Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype ArrivalWeight = ArrivalWeight Rational
    deriving (Eq, Ord, Show)

instance Newtype ArrivalWeight Rational where
    pack = ArrivalWeight
    unpack (ArrivalWeight a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''ArrivalWeight
deriveJsonViaSci ''ArrivalWeight
