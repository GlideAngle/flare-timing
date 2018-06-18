{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Ratio.Goal (NominalGoal(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefDec, deriveViaSci)

newtype NominalGoal = NominalGoal Rational
    deriving (Eq, Ord, Show)

instance Newtype NominalGoal Rational where
    pack = NominalGoal
    unpack (NominalGoal a) = a

deriveDefDec 8 ''NominalGoal
deriveViaSci ''NominalGoal
