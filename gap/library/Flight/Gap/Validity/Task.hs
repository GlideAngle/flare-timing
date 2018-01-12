{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flight.Gap.Validity.Task (TaskValidity(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), ViaSci(..))

-- | Also called Day Quality.
newtype TaskValidity = TaskValidity Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces TaskValidity where
    defdp _ = DecimalPlaces 8

instance Newtype TaskValidity Rational where
    pack = TaskValidity
    unpack (TaskValidity a) = a

instance ToJSON TaskValidity where
    toJSON x = toJSON $ ViaSci x

instance FromJSON TaskValidity where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x
