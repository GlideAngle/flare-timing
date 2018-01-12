{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flight.Gap.Weight.Arrival (ArrivalWeight(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), ViaSci(..))

newtype ArrivalWeight = ArrivalWeight Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces ArrivalWeight where
    defdp _ = DecimalPlaces 8

instance Newtype ArrivalWeight Rational where
    pack = ArrivalWeight
    unpack (ArrivalWeight a) = a

instance ToJSON ArrivalWeight where
    toJSON x = toJSON $ ViaSci x

instance FromJSON ArrivalWeight where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x
