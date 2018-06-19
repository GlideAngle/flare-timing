{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Weight.Leading (LeadingWeight(..)) where

import Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype LeadingWeight = LeadingWeight Rational
    deriving (Eq, Ord, Show)

instance Newtype LeadingWeight Rational where
    pack = LeadingWeight
    unpack (LeadingWeight a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''LeadingWeight
deriveJsonViaSci ''LeadingWeight
