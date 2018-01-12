{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flight.Gap.Weight.Leading (LeadingWeight(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), ViaSci(..))

newtype LeadingWeight = LeadingWeight Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces LeadingWeight where
    defdp _ = DecimalPlaces 8

instance Newtype LeadingWeight Rational where
    pack = LeadingWeight
    unpack (LeadingWeight a) = a

instance ToJSON LeadingWeight where
    toJSON x = toJSON $ ViaSci x

instance FromJSON LeadingWeight where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x
