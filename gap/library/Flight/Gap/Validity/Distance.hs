{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flight.Gap.Validity.Distance (DistanceValidity(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), ViaSci(..))

newtype DistanceValidity = DistanceValidity Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces DistanceValidity where
    defdp _ = DecimalPlaces 8

instance Newtype DistanceValidity Rational where
    pack = DistanceValidity
    unpack (DistanceValidity a) = a

instance ToJSON DistanceValidity where
    toJSON x = toJSON $ ViaSci x

instance FromJSON DistanceValidity where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x
