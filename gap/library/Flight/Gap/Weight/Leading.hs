{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Weight.Leading (LeadingWeight(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefDec, deriveViaSci)

newtype LeadingWeight = LeadingWeight Rational
    deriving (Eq, Ord, Show)

instance Newtype LeadingWeight Rational where
    pack = LeadingWeight
    unpack (LeadingWeight a) = a

deriveDefDec 8 ''LeadingWeight
deriveViaSci ''LeadingWeight
