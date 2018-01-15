{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Flight.Gap.Weight.GoalRatio (GoalRatio(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson.Via.Scientific (deriveDefaultDecimalPlaces, deriveViaSci)

-- | Pilots in goal versus pilots flying.
newtype GoalRatio = GoalRatio Rational
    deriving (Eq, Ord, Show)

instance Newtype GoalRatio Rational where
    pack = GoalRatio
    unpack (GoalRatio a) = a

deriveDefaultDecimalPlaces 8 ''GoalRatio
deriveViaSci ''GoalRatio
