{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Data.Aeson.Via.UnitsOfMeasure (ViaQ(..)) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Scientific (Scientific)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (Unpack, KnownUnit, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Show (showQuantity)
import Data.UnitsOfMeasure.Read (QuantityWithUnit(..), Some(..), readQuantity)

import Flight.Units ()
import Data.Aeson.Via.Scientific (DefaultDecimalPlaces(..), fromSci, toSci)

data ViaQ n a u where
    ViaQ
        :: (DefaultDecimalPlaces n, Newtype n (Quantity a u))
        => n
        -> ViaQ n a u

instance
    ( DefaultDecimalPlaces n
    , Newtype n (Quantity a u)
    , Real a
    , KnownUnit (Unpack u)
    )
    => ToJSON (ViaQ n a u) where
    toJSON (ViaQ x) = toJSON . showQuantity $ y
         where
             MkQuantity a = toRational' . unpack $ x

             y :: Quantity Scientific u
             y = MkQuantity . toSci (defdp x) $ a

instance
    ( DefaultDecimalPlaces n
    , Newtype n (Quantity a u)
    , Real a
    , Fractional a
    , KnownUnit (Unpack u)
    )
    => FromJSON (ViaQ n a u) where
    parseJSON o = do
        s :: String <- parseJSON o
        either
            fail
            (return . ViaQ . pack . fromRational' . MkQuantity . fromSci . unSome)
            (readQuantity s)

unSome :: Some (QuantityWithUnit p) -> p
unSome (Some (QuantityWithUnit (MkQuantity q) _)) = q
