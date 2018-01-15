{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flight.Gap.Ratio.Launch (NominalLaunch(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), ViaSci(..))

newtype NominalLaunch = NominalLaunch Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces NominalLaunch where
    defdp _ = DecimalPlaces 8

instance Newtype NominalLaunch Rational where
    pack = NominalLaunch
    unpack (NominalLaunch a) = a

instance ToJSON NominalLaunch where
    toJSON x = toJSON $ ViaSci x

instance FromJSON NominalLaunch where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x
