{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Flight.Gap.Time.Nominal (NominalTime(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Aeson.Via.UnitsOfMeasure (ViaQ(..))

newtype NominalTime a = NominalTime a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| h |])
    => DefaultDecimalPlaces (NominalTime q) where
    defdp _ = DecimalPlaces 6

instance
    (q ~ Quantity Double [u| h |])
    => Newtype (NominalTime q) q where
    pack = NominalTime
    unpack (NominalTime a) = a

instance (q ~ Quantity Double [u| h |]) => ToJSON (NominalTime q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| h |]) => FromJSON (NominalTime q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
