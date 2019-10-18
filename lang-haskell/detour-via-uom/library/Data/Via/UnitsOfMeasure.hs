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
import Data.Csv (ToField(..), FromField(..))
import Data.UnitsOfMeasure (Unpack, KnownUnit, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Show (showUnit)
import Data.UnitsOfMeasure.Read (QuantityWithUnit(..), Some(..), readQuantity)

import Data.Via.Scientific (DefaultDecimalPlaces(..), fromSci, toSci, showSci)

-- TODO: Reduce duplication in Data.Via.UnitsOfMeasure
-- Suggestion: Reduce duplication
-- Found:
--  MkQuantity a = toRational' . unpack $ x
--  y :: Quantity Scientific u
--  y = MkQuantity . toSci (defdp x) $ a

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

showQ
    :: forall n a u.
        ( DefaultDecimalPlaces n
        , Newtype n (Quantity a u)
        , Real a
        , KnownUnit (Unpack u)
        )
    => n
    -> String
showQ x =
    showSci (defdp x) y ++ if s == "1" then "" else ' ' : s
    where
        s = showUnit (undefined :: proxy u)
        MkQuantity y = toQ x

toQ
    ::
        ( DefaultDecimalPlaces n
        , Newtype n (Quantity a u)
        , Real a
        , KnownUnit (Unpack u)
        )
    => n
    -> Quantity Scientific u
toQ x = y
 where
     MkQuantity a = toRational' . unpack $ x

     y :: Quantity Scientific u
     y = MkQuantity . toSci (defdp x) $ a

instance
    ( DefaultDecimalPlaces n
    , Newtype n (Quantity a u)
    , Real a
    , KnownUnit (Unpack u)
    )
    => Show (ViaQ n a u) where
    show (ViaQ x) = show . showQ $ x

instance
    ( DefaultDecimalPlaces n
    , Newtype n (Quantity a u)
    , Real a
    , KnownUnit (Unpack u)
    )
    => ToJSON (ViaQ n a u) where
    toJSON (ViaQ x) = toJSON . showQ $ x

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

instance
    ( DefaultDecimalPlaces n
    , Newtype n (Quantity a u)
    , Real a
    , KnownUnit (Unpack u)
    )
    => ToField (ViaQ n a u) where
    toField (ViaQ x) = toField . showQ $ x

instance
    ( DefaultDecimalPlaces n
    , Newtype n (Quantity a u)
    , Real a
    , Fractional a
    , KnownUnit (Unpack u)
    )
    => FromField (ViaQ n a u) where
    parseField o = do
        s :: String <- parseField o
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
-- >>> import Data.Aeson (ToJSON(..), FromJSON(..), encode, decode)
-- >>> import Data.Csv as Csv (ToField(..), FromField(..), HasHeader(..))
-- >>> import qualified Data.Csv as Csv (encode, decode)
-- >>> import Data.Vector (Vector, fromList)
-- >>> import Control.Newtype (Newtype(..))
-- >>> import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
-- >>> import Data.UnitsOfMeasure (u, Quantity(..))

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
-- :}
-- 
-- Encoding and decoding JSON.
--
-- >>> :{
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
-- 
-- >>> [u| -0.038 km |]
-- [u| -3.8e-2 km |]
-- >>> encode (Distance [u| -0.038 km |])
-- "\"-0.038 km\""
-- >>> let Just x :: Maybe (Distance (Quantity Double [u| km |])) = decode (encode (Distance [u| -0.038 km |])) in x
-- Distance [u| -3.8e-2 km |]
-- 
-- Similarly for CSV.
--
-- >>> :{
-- instance (q ~ Quantity Double [u| km |]) => ToField (Distance q) where
--     toField x = toField $ ViaQ x
-- instance (q ~ Quantity Double [u| km |]) => FromField (Distance q) where
--     parseField c = do ViaQ x <- parseField c; return x
-- :}
-- 
-- >>> let d = Distance [u| 112233.445566 km |]
-- >>> Csv.encode [("A", d)]
-- "A,112233.446 km\r\n"
-- >>> Csv.decode NoHeader (Csv.encode [("B", d)]) == Right (fromList [("B", d)])
-- False
-- >>> Csv.decode NoHeader (Csv.encode [("C", d)]) == Right (fromList [("C", Distance [u| 112233.446 km |])])
-- True
