{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Validity.Time (TimeValidity(..)) where

import Control.Newtype (Newtype(..))
import Data.Via.Scientific (deriveDefDec, deriveViaSci)

newtype TimeValidity = TimeValidity Rational
    deriving (Eq, Ord, Show)

instance Newtype TimeValidity Rational where
    pack = TimeValidity
    unpack (TimeValidity a) = a

deriveDefDec 8 ''TimeValidity
deriveViaSci ''TimeValidity
