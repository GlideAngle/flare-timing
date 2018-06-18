{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Points.Task (TaskPoints(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefDec, deriveViaSci)

newtype TaskPoints = TaskPoints Rational
    deriving (Eq, Ord, Show)

instance Newtype TaskPoints Rational where
    pack = TaskPoints
    unpack (TaskPoints a) = a

deriveDefDec 0 ''TaskPoints
deriveViaSci ''TaskPoints
