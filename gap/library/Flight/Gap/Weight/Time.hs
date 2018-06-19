{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Weight.Time (TimeWeight(..)) where

import Control.Newtype (Newtype(..))
import Data.Via.Scientific (deriveDefDec, deriveViaSci)

newtype TimeWeight = TimeWeight Rational
    deriving (Eq, Ord, Show)

instance Newtype TimeWeight Rational where
    pack = TimeWeight
    unpack (TimeWeight a) = a

deriveDefDec 8 ''TimeWeight
deriveViaSci ''TimeWeight
