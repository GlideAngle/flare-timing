{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flight.Gap.Ratio.Speed (SpeedFraction(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), ViaSci(..))

newtype SpeedFraction = SpeedFraction Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces SpeedFraction where
    defdp _ = DecimalPlaces 8

instance Newtype SpeedFraction Rational where
    pack = SpeedFraction
    unpack (SpeedFraction a) = a

instance ToJSON SpeedFraction where
    toJSON x = toJSON $ ViaSci x

instance FromJSON SpeedFraction where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x
