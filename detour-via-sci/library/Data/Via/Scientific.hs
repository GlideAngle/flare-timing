{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

{-|
Module      : Data.Via.Scientific
Copyright   : (c) Block Scope Limited 2018
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

For encoding and decoding newtype rationals as scientific with a fixed number
of decimal places.
-}
module Data.Via.Scientific
    (
    -- * Usage
    -- $use
    
    -- * Decimal Places
      DecimalPlaces(..)
    , ViaSci(..)
    , DefaultDecimalPlaces(..)
    , dpDegree
    , showSci
    -- * Conversions
    , fromSci
    , toSci
    -- * Deriving instances with Template Haskell
    , deriveDecimalPlaces
    , deriveJsonViaSci
    , deriveCsvViaSci
    ) where

import Control.Newtype (Newtype(..))
import Control.Applicative (empty)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(Number))
import Data.Csv (ToField(..), FromField(..))
import Data.Scientific
    ( Scientific
    , FPFormat(..)
    , toRealFloat
    , fromRationalRepetend
    , formatScientific
    )
import Language.Haskell.TH (Q, Name, Dec, conT)
import Language.Haskell.TH.Syntax

-- | A choice of 8 decimal places for
-- <https://en.wikipedia.org/wiki/Decimal_degrees decimal degrees> is just
-- a bit more than a mm at the equator and less elsewhere.
--
--     * 1.1132 mm at the equator
--     * 1.0247 mm at 23° N/S
--     * 787.1 µm at 45° N/S
--     * 434.96 µm at 67° N/S
dpDegree :: DecimalPlaces
dpDegree = DecimalPlaces 8

-- NOTE: For deriving Lift, see https://ghc.haskell.org/trac/ghc/ticket/14296
-- | A positive number of decimal places.
newtype DecimalPlaces = DecimalPlaces Int deriving (Show, Lift)

-- | From 'Scientific' exactly to 'Rational'.
fromSci :: Scientific -> Rational
fromSci x = toRational (toRealFloat x :: Double)

-- | To 'Scientific' from 'Rational' as near as possible up to the given number
-- of 'DecimalPlaces'.
toSci :: DecimalPlaces -> Rational -> Scientific
toSci (DecimalPlaces dp) x =
    case fromRationalRepetend (Just $ dp + 1) x of
        Left (s, _) -> s
        Right (s, _) -> s

-- | Shows a 'Scientific' value with a fixed number of decimal places.
showSci :: DecimalPlaces -> Scientific -> String
showSci (DecimalPlaces dp) =
    formatScientific Fixed (Just dp)

-- | A default number of decimal places for a type.
class DefaultDecimalPlaces a where
    defdp :: a -> DecimalPlaces
    defdp _ = DecimalPlaces 0

-- | An intermediate type used during encoding to JSON with @aeson@ and during
-- encoding to CSV with @cassava@. It's also used during decoding.
--
-- The original type, a newtype 'Rational', goes to and fro __via__
-- __sci__entific so that the rational value can be encoded as a scientific
-- value with a fixed number of decimal places.
data ViaSci n where
    ViaSci
        :: (DefaultDecimalPlaces n, Newtype n Rational)
        => n 
        -> ViaSci n

deriving instance (Eq n) => Eq (ViaSci n)
deriving instance (Ord n) => Ord (ViaSci n)
deriving instance (Show n) => Show (ViaSci n)

instance
    (DefaultDecimalPlaces n, Newtype n Rational)
    => ToJSON (ViaSci n) where
    toJSON (ViaSci x) = Number $ toSci (defdp x) (unpack x)

instance
    (DefaultDecimalPlaces n, Newtype n Rational)
    => FromJSON (ViaSci n) where
    parseJSON x@(Number _) = ViaSci <$> (pack . fromSci <$> parseJSON x)
    parseJSON _ = empty

instance
    (DefaultDecimalPlaces n, Newtype n Rational)
    => ToField (ViaSci n) where
    toField (ViaSci x) = toField $ toSci (defdp x) (unpack x)

instance
    (DefaultDecimalPlaces n, Newtype n Rational)
    => FromField (ViaSci n) where
    parseField x = ViaSci <$> (pack . fromSci <$> parseField x)

-- SEE: https://markkarpov.com/tutorial/th.html
-- | Taking a number of decimal places from the given 'DecimalPlaces' newtype,
-- derives an instance of 'DefaultDecimalPlaces'.
deriveDecimalPlaces :: DecimalPlaces -> Name -> Q [Dec]
deriveDecimalPlaces dp name =
    [d|
        instance DefaultDecimalPlaces $(conT name) where
            defdp _ = $(lift dp)
        |]

-- | Derives an instance of 'ToJSON' wrapping the value with 'ViaSci' before
-- encoding. Similarly the value is decoded as 'ViaSci' and then unwrapped in
-- the derived instance of 'FromJSON'.
deriveJsonViaSci :: Name -> Q [Dec]
deriveJsonViaSci name =
    [d|
        instance ToJSON $a where
            toJSON x = toJSON $ ViaSci x

        instance FromJSON $a where
            parseJSON o = do
                ViaSci x <- parseJSON o
                return x
        |]
    where
        a = conT name

-- | Similar to 'deriveJsonViaSci' but for instances of 'ToField' and 'FromField'.
deriveCsvViaSci :: Name -> Q [Dec]
deriveCsvViaSci name =
    [d|
        instance ToField $a where
            toField = toField . ViaSci

        instance FromField $a where
            parseField c = do
                ViaSci x <- parseField c
                return x
        |]
    where
        a = conT name

-- $use
-- When having to check numbers by hand, a fixed decimal is more familiar than
-- a ratio of possibly large integers.
-- 
-- Let's say we have a latitude that is a @newtype@ 'Rational' number but we
-- want it to be encoded to JSON with a fixed number of decimal places.
--
-- @
-- newtype Lat = Lat Rational deriving (Eq, Ord, Show)
-- @
--
-- Derive instances of 'DefaultDecimalPlaces' and 'ViaSci'.
--
-- @ 
-- deriveDecimalPlaces (DecimalPlaces 8) ''Lat
-- deriveJsonViaSci ''Lat
-- @
--
-- Types going 'ViaSci' also need to be instances of 'Newtype'.
-- 
-- @
-- instance Newtype Lat Rational where
--     pack = Lat
--     unpack (Lat a) = a
-- @
-- > >>> import Data.Ratio ((%))
-- > >>> import Data.Aeson (encode)
-- > >>> let angle = 1122334455667788 % 10000000000000000
-- > >>> fromRational angle
-- > 0.1122334455667788
-- > >>> let lat = Lat angle
-- > >>> encode angle
-- > "{\"numerator\":280583613916947,\"denominator\":2500000000000000}"
-- > >>> encode lat
-- > "0.11223344"
