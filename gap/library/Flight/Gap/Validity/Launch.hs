{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Validity.Launch (LaunchValidity(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefaultDecimalPlaces, deriveViaSci)

newtype LaunchValidity = LaunchValidity Rational
    deriving (Eq, Ord, Show)

instance Newtype LaunchValidity Rational where
    pack = LaunchValidity
    unpack (LaunchValidity a) = a

deriveDefaultDecimalPlaces 8 ''LaunchValidity
deriveViaSci ''LaunchValidity
