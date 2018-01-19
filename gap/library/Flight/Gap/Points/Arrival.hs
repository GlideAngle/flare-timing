{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Points.Arrival (ArrivalPoints(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefDec, deriveViaSci)

newtype ArrivalPoints = ArrivalPoints Rational
    deriving (Eq, Ord, Show)

instance Newtype ArrivalPoints Rational where
    pack = ArrivalPoints
    unpack (ArrivalPoints a) = a

deriveDefDec 0 ''ArrivalPoints
deriveViaSci ''ArrivalPoints
