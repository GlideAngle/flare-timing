{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Flight.Gap.Distance.Fraction (DifficultyFraction(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), ViaSci(..))

-- | The sum of relative difficulties up until the chunk of landing.
newtype DifficultyFraction = DifficultyFraction Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces DifficultyFraction where
    defdp _ = DecimalPlaces 8

instance Newtype DifficultyFraction Rational where
    pack = DifficultyFraction
    unpack (DifficultyFraction a) = a

instance ToJSON DifficultyFraction where
    toJSON x = toJSON $ ViaSci x

instance FromJSON DifficultyFraction where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x
