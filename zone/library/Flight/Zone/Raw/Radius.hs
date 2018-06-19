{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Flight.Zone.Raw.Radius (RawRadius(..), showRadius) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (KnownUnit, Unpack, u)
import Data.UnitsOfMeasure.Show (showQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

newtype RawRadius a = RawRadius a
    deriving (Eq, Ord, Show)

showRadius :: (Show a, KnownUnit (Unpack u)) => RawRadius (Quantity a u) -> String
showRadius (RawRadius r) = showQuantity r

instance
    (q ~ Quantity Double [u| m |])
    => DefaultDecimalPlaces (RawRadius q) where
    defdp _ = DecimalPlaces 1

instance
    (q ~ Quantity Double [u| m |])
    => Newtype (RawRadius q) q where
    pack = RawRadius
    unpack (RawRadius a) = a

instance (q ~ Quantity Double [u| m |]) => ToJSON (RawRadius q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| m |]) => FromJSON (RawRadius q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
