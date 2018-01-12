{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flight.Gap.Weight.GoalRatio (GoalRatio(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), ViaSci(..))

-- | Pilots in goal versus pilots flying.
newtype GoalRatio = GoalRatio Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces GoalRatio where
    defdp _ = DecimalPlaces 8

instance Newtype GoalRatio Rational where
    pack = GoalRatio
    unpack (GoalRatio a) = a

instance ToJSON GoalRatio where
    toJSON x = toJSON $ ViaSci x

instance FromJSON GoalRatio where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x
