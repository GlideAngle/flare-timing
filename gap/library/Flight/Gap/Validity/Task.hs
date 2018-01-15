{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Validity.Task (TaskValidity(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefDec, deriveViaSci)

-- | Also called Day Quality.
newtype TaskValidity = TaskValidity Rational
    deriving (Eq, Ord, Show)

instance Newtype TaskValidity Rational where
    pack = TaskValidity
    unpack (TaskValidity a) = a

deriveDefDec 8 ''TaskValidity
deriveViaSci ''TaskValidity
