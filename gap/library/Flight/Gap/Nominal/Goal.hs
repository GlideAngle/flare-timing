{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flight.Gap.Nominal.Goal (NominalGoal(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), ViaSci(..))

newtype NominalGoal = NominalGoal Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces NominalGoal where
    defdp _ = DecimalPlaces 8

instance Newtype NominalGoal Rational where
    pack = NominalGoal
    unpack (NominalGoal a) = a

instance ToJSON NominalGoal where
    toJSON x = toJSON $ ViaSci x

instance FromJSON NominalGoal where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x
