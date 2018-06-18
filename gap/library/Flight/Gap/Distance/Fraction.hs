{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Distance.Fraction (DifficultyFraction(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefDec, deriveViaSci)

-- | The sum of relative difficulties up until the chunk of landing.
newtype DifficultyFraction = DifficultyFraction Rational
    deriving (Eq, Ord, Show)

instance Newtype DifficultyFraction Rational where
    pack = DifficultyFraction
    unpack (DifficultyFraction a) = a

deriveDefDec 8 ''DifficultyFraction
deriveViaSci ''DifficultyFraction
