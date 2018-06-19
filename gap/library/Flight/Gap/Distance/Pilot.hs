{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Flight.Gap.Distance.Pilot (PilotDistance(..)) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))

newtype PilotDistance a = PilotDistance a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| km |])
    => DefaultDecimalPlaces (PilotDistance q) where
    defdp _ = DecimalPlaces 1

instance
    (q ~ Quantity Double [u| km |])
    => Newtype (PilotDistance q) q where
    pack = PilotDistance
    unpack (PilotDistance a) = a

instance (q ~ Quantity Double [u| km |]) => ToJSON (PilotDistance q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| km |]) => FromJSON (PilotDistance q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
