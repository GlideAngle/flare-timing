{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Ratio.Leading
    ( LeadingAreaStep(..)
    , LeadingCoefficient(..)
    , LeadingFraction(..)
    , EssTime(..)
    ) where

import Control.Newtype (Newtype(..))
import Data.Via.Scientific (deriveDefDec, deriveViaSci, deriveCsvViaSci)

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
deriveCsvViaSci ''LeadingCoefficient

newtype LeadingFraction = LeadingFraction Rational
    deriving (Eq, Ord, Show)

instance Newtype LeadingFraction Rational where
    pack = LeadingFraction
    unpack (LeadingFraction a) = a

deriveDefDec 8 ''LeadingFraction
deriveViaSci ''LeadingFraction
deriveCsvViaSci ''LeadingFraction

-- | Task time when the last pilot made the end of the speed section.
newtype EssTime = EssTime Rational
    deriving (Eq, Ord, Show)

instance Newtype EssTime Rational where
    pack = EssTime
    unpack (EssTime a) = a

deriveDefDec 3 ''EssTime
deriveViaSci ''EssTime
deriveCsvViaSci ''EssTime
