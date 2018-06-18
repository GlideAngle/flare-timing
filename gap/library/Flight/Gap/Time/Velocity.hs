{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Flight.Gap.Time.Velocity (PilotVelocity(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Aeson.Via.UnitsOfMeasure (ViaQ(..))

newtype PilotVelocity a = PilotVelocity a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| km / h |])
    => DefaultDecimalPlaces (PilotVelocity q) where
    defdp _ = DecimalPlaces 2

instance
    (q ~ Quantity Double [u| km / h |])
    => Newtype (PilotVelocity q) q where
    pack = PilotVelocity
    unpack (PilotVelocity a) = a

instance (q ~ Quantity Double [u| km / h |]) => ToJSON (PilotVelocity q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| km / h |]) => FromJSON (PilotVelocity q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
