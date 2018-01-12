{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Flight.Gap.Weighting.GoalRatio (GoalRatio(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

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
