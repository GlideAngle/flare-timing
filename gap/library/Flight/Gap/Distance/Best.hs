{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Flight.Gap.Distance.Best (BestDistance(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Aeson.Via.UnitsOfMeasure (ViaQ(..))

newtype BestDistance a = BestDistance a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| km |])
    => DefaultDecimalPlaces (BestDistance q) where
    defdp _ = DecimalPlaces 1

instance
    (q ~ Quantity Double [u| km |])
    => Newtype (BestDistance q) q where
    pack = BestDistance
    unpack (BestDistance a) = a

instance (q ~ Quantity Double [u| km |]) => ToJSON (BestDistance q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| km |]) => FromJSON (BestDistance q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
