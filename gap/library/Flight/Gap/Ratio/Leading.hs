{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Ratio.Leading
    ( LeadingAreaStep(..)
    , LeadingCoefficient(..)
    , LeadingFraction(..)
    ) where

import Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefDec, deriveViaSci, deriveCsvViaSci)

newtype LeadingAreaStep = LeadingAreaStep Rational
    deriving (Eq, Ord, Show)

instance Newtype LeadingAreaStep Rational where
    pack = LeadingAreaStep
    unpack (LeadingAreaStep a) = a

deriveDefDec 8 ''LeadingAreaStep
deriveViaSci ''LeadingAreaStep
deriveCsvViaSci ''LeadingAreaStep

newtype LeadingCoefficient = LeadingCoefficient Rational
    deriving (Eq, Ord, Show)

instance Newtype LeadingCoefficient Rational where
    pack = LeadingCoefficient
    unpack (LeadingCoefficient a) = a

deriveDefDec 8 ''LeadingCoefficient
deriveViaSci ''LeadingCoefficient

newtype LeadingFraction = LeadingFraction Rational
    deriving (Eq, Ord, Show)

instance Newtype LeadingFraction Rational where
    pack = LeadingFraction
    unpack (LeadingFraction a) = a

deriveDefDec 8 ''LeadingFraction
deriveViaSci ''LeadingFraction
