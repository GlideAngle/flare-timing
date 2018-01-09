{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Gap.Distance.MinMax
    ( MinimumDistance(..)
    , MaximumDistance(..)
    , SumOfDistance(..)
    ) where

import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Data.Aeson.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Aeson.Via.UnitsOfMeasure (ViaQ(..))

newtype MinimumDistance a = MinimumDistance a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| km |])
    => DefaultDecimalPlaces (MinimumDistance q) where
    defdp _ = DecimalPlaces 1

instance
    (q ~ Quantity Double [u| km |])
    => Newtype (MinimumDistance q) q where
    pack = MinimumDistance
    unpack (MinimumDistance a) = a

instance (q ~ Quantity Double [u| km |]) => ToJSON (MinimumDistance q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| km |]) => FromJSON (MinimumDistance q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x

newtype MaximumDistance a = MaximumDistance a
    deriving (Eq, Ord, Show)

instance
    (q ~ Quantity Double [u| km |])
    => DefaultDecimalPlaces (MaximumDistance q) where
    defdp _ = DecimalPlaces 1

instance
    (q ~ Quantity Double [u| km |])
    => Newtype (MaximumDistance q) q where
    pack = MaximumDistance
    unpack (MaximumDistance a) = a

instance (q ~ Quantity Double [u| km |]) => ToJSON (MaximumDistance q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| km |]) => FromJSON (MaximumDistance q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x

newtype SumOfDistance = SumOfDistance (Quantity Double [u| km |])
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces SumOfDistance where
    defdp _ = DecimalPlaces 1

instance (u ~ [u| km |]) => Newtype SumOfDistance (Quantity Double u) where
    pack = SumOfDistance
    unpack (SumOfDistance a) = a

instance ToJSON SumOfDistance where
    toJSON x = toJSON $ ViaQ x

instance FromJSON SumOfDistance where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x
