{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Validity.Launch (LaunchValidity(..)) where

import Control.Newtype (Newtype(..))
import Data.Via.Scientific (DecimalPlaces(..), deriveDecimalPlaces, deriveJsonViaSci)

newtype LaunchValidity = LaunchValidity Rational
    deriving (Eq, Ord, Show)

instance Newtype LaunchValidity Rational where
    pack = LaunchValidity
    unpack (LaunchValidity a) = a

deriveDecimalPlaces (DecimalPlaces 8) ''LaunchValidity
deriveJsonViaSci ''LaunchValidity
