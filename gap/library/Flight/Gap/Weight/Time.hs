{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flight.Gap.Weight.Time (TimeWeight(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), ViaSci(..))

newtype TimeWeight = TimeWeight Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces TimeWeight where
    defdp _ = DecimalPlaces 8

instance Newtype TimeWeight Rational where
    pack = TimeWeight
    unpack (TimeWeight a) = a

instance ToJSON TimeWeight where
    toJSON x = toJSON $ ViaSci x

instance FromJSON TimeWeight where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x
