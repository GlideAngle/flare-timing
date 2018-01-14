{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flight.Gap.Weight.Distance (DistanceWeight(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), ViaSci(..))

newtype DistanceWeight = DistanceWeight Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces DistanceWeight where
    defdp _ = DecimalPlaces 8

instance Newtype DistanceWeight Rational where
    pack = DistanceWeight
    unpack (DistanceWeight a) = a

instance ToJSON DistanceWeight where
    toJSON x = toJSON $ ViaSci x

instance FromJSON DistanceWeight where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x
