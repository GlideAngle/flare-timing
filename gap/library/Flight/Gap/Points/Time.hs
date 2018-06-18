{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Points.Time (TimePoints(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefDec, deriveViaSci)

newtype TimePoints = TimePoints Rational
    deriving (Eq, Ord, Show)

instance Newtype TimePoints Rational where
    pack = TimePoints
    unpack (TimePoints a) = a

deriveDefDec 1 ''TimePoints
deriveViaSci ''TimePoints
