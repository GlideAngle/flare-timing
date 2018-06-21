{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-|
Module: Data.Via.UnitsOfMeasure
Copyright:
    © 2018 Phil de Joux
    © 2018 Block Scope Limited
License: MPL-2.0
Maintainer: Phil de Joux <phil.dejoux@blockscope.com>
Stability: experimental

For encoding and decoding newtype quantities as scientific with a fixed number
of decimal places and with units.
-}
module Data.Via.UnitsOfMeasure
    (
    -- * Usage
    -- $use
    
    -- * Decimal Places and Units
      ViaQ(..)
    ) where

import Control.Newtype (Newtype(..))
import Data.Scientific (Scientific)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (Unpack, KnownUnit, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Show (showQuantity)
import Data.UnitsOfMeasure.Read (QuantityWithUnit(..), Some(..), readQuantity)

import Data.Via.Scientific (DefaultDecimalPlaces(..), fromSci, toSci)

-- | An intermediate type used during encoding to JSON with @aeson@ and during
-- encoding to CSV with @cassava@. It's also used during decoding.
--
-- The original type, a newtype 'Quantity', goes to and fro __via__ this
-- __q__uantity so that the rational value can be encoded as a scientific value
-- with a fixed number of decimal places and with units.
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

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XQuasiQuotes
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XScopedTypeVariables
-- >>> :set -XTypeOperators
-- >>> :set -XTypeFamilies
-- >>> :set -XUndecidableInstances
-- >>> import Data.Aeson
-- >>> import Control.Newtype (Newtype(..))
-- >>> import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
-- >>> import Data.UnitsOfMeasure
-- >>> import Data.UnitsOfMeasure.Show

-- $use
-- With these unit definitons;
-- 
-- @
-- [u| m |]
-- [u| km = 1000 m |]
-- @
-- 
-- Let's say we have distances in kilometres we'd like encoded with 3 decimal
-- places.
--
-- >>> :{
-- newtype Distance a = Distance a deriving (Eq, Ord, Show)
-- instance (q ~ Quantity Double [u| km |]) => DefaultDecimalPlaces (Distance q) where
--     defdp _ = DecimalPlaces 3
-- instance (q ~ Quantity Double [u| km |]) => Newtype (Distance q) q where
--     pack = Distance
--     unpack (Distance a) = a
-- instance (q ~ Quantity Double [u| km |]) => ToJSON (Distance q) where
--     toJSON x = toJSON $ ViaQ x
-- instance (q ~ Quantity Double [u| km |]) => FromJSON (Distance q) where
--     parseJSON o = do ViaQ x <- parseJSON o; return x
-- :}
-- 
-- >>> [u| 112233.445566 km |]
-- [u| 112233.445566 km |]
-- >>> encode (Distance [u| 112233.445566 km |])
-- "\"112233.446 km\""
-- >>> let Just x :: Maybe (Distance (Quantity Double [u| km |])) = decode (encode (Distance [u| 112233.445566 km |])) in x
-- Distance [u| 112233.446 km |]
