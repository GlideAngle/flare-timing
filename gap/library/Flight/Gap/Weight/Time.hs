{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Weight.Time (TimeWeight(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefaultDecimalPlaces, deriveViaSci)

newtype TimeWeight = TimeWeight Rational
    deriving (Eq, Ord, Show)

instance Newtype TimeWeight Rational where
    pack = TimeWeight
    unpack (TimeWeight a) = a

deriveDefaultDecimalPlaces 8 ''TimeWeight
deriveViaSci ''TimeWeight
