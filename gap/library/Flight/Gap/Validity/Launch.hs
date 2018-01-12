{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flight.Gap.Validity.Launch (LaunchValidity(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), ViaSci(..))

newtype LaunchValidity = LaunchValidity Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces LaunchValidity where
    defdp _ = DecimalPlaces 8

instance Newtype LaunchValidity Rational where
    pack = LaunchValidity
    unpack (LaunchValidity a) = a

instance ToJSON LaunchValidity where
    toJSON x = toJSON $ ViaSci x

instance FromJSON LaunchValidity where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x
