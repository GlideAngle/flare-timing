{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Weight.Arrival (ArrivalWeight(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefaultDecimalPlaces, deriveViaSci)

newtype ArrivalWeight = ArrivalWeight Rational
    deriving (Eq, Ord, Show)

instance Newtype ArrivalWeight Rational where
    pack = ArrivalWeight
    unpack (ArrivalWeight a) = a

deriveDefaultDecimalPlaces 8 ''ArrivalWeight
deriveViaSci ''ArrivalWeight
