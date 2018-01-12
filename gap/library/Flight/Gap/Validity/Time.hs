{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flight.Gap.Validity.Time (TimeValidity(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), ViaSci(..))

newtype TimeValidity = TimeValidity Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces TimeValidity where
    defdp _ = DecimalPlaces 8

instance Newtype TimeValidity Rational where
    pack = TimeValidity
    unpack (TimeValidity a) = a

instance ToJSON TimeValidity where
    toJSON x = toJSON $ ViaSci x

instance FromJSON TimeValidity where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x
