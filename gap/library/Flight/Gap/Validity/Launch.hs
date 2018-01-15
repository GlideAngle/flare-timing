{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Validity.Launch (LaunchValidity(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefDec, deriveViaSci)

newtype LaunchValidity = LaunchValidity Rational
    deriving (Eq, Ord, Show)

instance Newtype LaunchValidity Rational where
    pack = LaunchValidity
    unpack (LaunchValidity a) = a

deriveDefDec 8 ''LaunchValidity
deriveViaSci ''LaunchValidity
