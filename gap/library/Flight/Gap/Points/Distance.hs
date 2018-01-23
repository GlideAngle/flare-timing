{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Points.Distance
    ( DistancePoints(..)
    , LinearPoints(..)
    , DifficultyPoints(..)
    ) where

import Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefDec, deriveViaSci)

newtype DistancePoints = DistancePoints Rational
    deriving (Eq, Ord, Show)

instance Newtype DistancePoints Rational where
    pack = DistancePoints
    unpack (DistancePoints a) = a

deriveDefDec 0 ''DistancePoints
deriveViaSci ''DistancePoints

newtype LinearPoints = LinearPoints Rational
    deriving (Eq, Ord, Show)

instance Newtype LinearPoints Rational where
    pack = LinearPoints
    unpack (LinearPoints a) = a

deriveDefDec 0 ''LinearPoints
deriveViaSci ''LinearPoints

newtype DifficultyPoints = DifficultyPoints Rational
    deriving (Eq, Ord, Show)

instance Newtype DifficultyPoints Rational where
    pack = DifficultyPoints
    unpack (DifficultyPoints a) = a

deriveDefDec 0 ''DifficultyPoints
deriveViaSci ''DifficultyPoints
