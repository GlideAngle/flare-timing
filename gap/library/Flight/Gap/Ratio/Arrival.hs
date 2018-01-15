{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flight.Gap.Ratio.Arrival (ArrivalFraction(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), ViaSci(..))

newtype ArrivalFraction = ArrivalFraction Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces ArrivalFraction where
    defdp _ = DecimalPlaces 8

instance Newtype ArrivalFraction Rational where
    pack = ArrivalFraction
    unpack (ArrivalFraction a) = a

instance ToJSON ArrivalFraction where
    toJSON x = toJSON $ ViaSci x

instance FromJSON ArrivalFraction where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x
