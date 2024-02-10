{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

{-|
Module: Data.Via.Scientific
Copyright:
    © 2018 Phil de Joux
    © 2018 Block Scope Limited
License: MPL-2.0
Maintainer: Phil de Joux <phil.dejoux@blockscope.com>
Stability: experimental

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
    , dpAlt
    , showSci
    -- * Conversions
    , fromSci
    , toSci
    -- * Deriving instances with Template Haskell
    , deriveDecimalPlaces
    , deriveShowValueViaSci
    , deriveShowViaSci
    , deriveJsonViaSci
    , deriveCsvViaSci
    ) where

import Data.Typeable (Typeable)
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
import Language.Haskell.TH (conT)
import Language.Haskell.TH.Syntax (Lift(..), Q, Name, Dec)
import Data.Ratio.Rounding (dpRound)

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

-- | A choice of decimal places for altitude in metres.
dpAlt :: DecimalPlaces
dpAlt = DecimalPlaces 2

-- NOTE: For deriving Lift, see https://ghc.haskell.org/trac/ghc/ticket/14296
-- | A positive number of decimal places.
newtype DecimalPlaces = DecimalPlaces Int deriving (Show, Lift)

-- | From 'Scientific' exactly to 'Rational'.
--
-- >>> let x = 0.1122334455667788
-- >>> fromSci x :: Rational
-- 4043636029064415 % 36028797018963968
-- >>> x == fromRational (fromSci x)
-- True
-- >>> let y = -0.038
-- >>> fromSci y :: Rational
-- (-5476377146882523) % 144115188075855872
-- >>> y == fromRational (fromSci y)
-- True
fromSci :: Fractional a => Scientific -> a
fromSci x = fromRational $ toRational (toRealFloat x :: Double)

-- | To 'Scientific' from 'Rational' as near as possible up to the given number
-- of 'DecimalPlaces' with rounding.
--
-- >>> let x = 1122334455667788 % 10000000000000000
-- >>> toSci (DecimalPlaces 8) x
-- 0.11223345
-- >>> x == toRational (toSci (DecimalPlaces 8) x)
-- False
-- >>> x == toRational (toSci (DecimalPlaces 16) x)
-- True
-- >>> x == toRational (toSci (DecimalPlaces 32) x)
-- True
toSci :: Real a => DecimalPlaces -> a -> Scientific
toSci (DecimalPlaces dp) x' =
    let x = toRational x'
        y = dpRound (toInteger dp) x

    in case fromRationalRepetend (Just $ dp + 1) y of
        Left (s, _) -> s
        Right (s, _) -> s

-- | Shows a 'Scientific' value with a fixed number of decimal places.
--
-- >>> let x = 0.1122334455667788
-- >>> showSci (DecimalPlaces 16) x
-- "0.1122334455667788"
-- >>> showSci (DecimalPlaces 8) x
-- "0.11223345"
-- >>> showSci (DecimalPlaces 4) x
-- "0.1122"
-- >>> showSci (DecimalPlaces 1) x
-- "0.1"
-- >>> showSci (DecimalPlaces 0) x
-- "0"
-- >>> showSci (DecimalPlaces (-1)) x
-- "0"
-- >>> showSci (DecimalPlaces 32) x
-- "0.11223344556677880000000000000000"
-- >>> showSci (DecimalPlaces 3) $ -0.038
-- "-0.038"
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
data ViaSci n d where
    ViaSci
        :: (DefaultDecimalPlaces n, Newtype n a)
        => n
        -> ViaSci n a

deriving instance (Eq n) => Eq (ViaSci n a)
deriving instance (Ord n) => Ord (ViaSci n a)

instance
    (DefaultDecimalPlaces n, Newtype n a, Real a)
    => Show (ViaSci n a) where
    show (ViaSci x) = show $ toSci (defdp x) (unpack x)

instance
    (DefaultDecimalPlaces n, Newtype n a, Real a)
    => ToJSON (ViaSci n a) where
    toJSON (ViaSci x) = Number $ toSci (defdp x) (unpack x)

instance
    (DefaultDecimalPlaces n, Newtype n a, Fractional a)
    => FromJSON (ViaSci n a) where
    parseJSON x@(Number _) = ViaSci <$> (pack . fromSci <$> parseJSON x)
    parseJSON _ = empty

instance
    (DefaultDecimalPlaces n, Newtype n a, Real a)
    => ToField (ViaSci n a) where
    toField (ViaSci x) = toField $ toSci (defdp x) (unpack x)

instance
    (DefaultDecimalPlaces n, Newtype n a, Fractional a)
    => FromField (ViaSci n a) where
    parseField x = ViaSci <$> (pack . fromSci <$> parseField x)

-- SEE: https://markkarpov.com/tutorial/th.html
-- | Taking a number of decimal places from the given 'DecimalPlaces' newtype,
-- derives an instance of 'DefaultDecimalPlaces'.
--
-- >>> deriveDecimalPlaces (DecimalPlaces 8) ''Lat
-- ...
deriveDecimalPlaces :: DecimalPlaces -> Name -> Q [Dec]
deriveDecimalPlaces dp name =
    [d|
        instance DefaultDecimalPlaces $(conT name) where
            defdp _ = $(lift dp)
        |]

-- | Derives an instance of 'Show' wrapping the value with 'ViaSci' before
-- showing.
--
-- >>> deriveShowValueViaSci ''Lat
-- ...
deriveShowValueViaSci :: Name -> Q [Dec]
deriveShowValueViaSci name =
    [d|
        instance Show $a where
            show x = show $ ViaSci x
        |]
    where
        a = conT name

-- | Derives an instance of 'Show' wrapping the value with 'ViaSci' before
-- showing and showing the type name. The type should derive @Typeable@.
--
-- >>> deriveShowViaSci ''Lat
-- ...
deriveShowViaSci :: Name -> Q [Dec]
deriveShowViaSci name =
    [d|
        instance Typeable $a => Show $a where
            show x = show (typeOf x) ++ " " ++ show (ViaSci x)
        |]
    where
        a = conT name

-- | Derives an instance of 'ToJSON' wrapping the value with 'ViaSci' before
-- encoding. Similarly the value is decoded as 'ViaSci' and then unwrapped in
-- the derived instance of 'FromJSON'.
--
-- >>> deriveJsonViaSci ''Lat
-- ...
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
--
-- >>> deriveCsvViaSci ''Lat
-- ...
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

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XTemplateHaskell
-- >>> :set -XFlexibleInstances
-- >>> :set -XMultiParamTypeClasses
-- >>> import Data.Ratio ((%))
-- >>> import Data.Aeson (encode, decode)
-- >>> import Data.Text (Text)
-- >>> import Data.Vector (Vector, fromList)
-- >>> import qualified Data.Csv as Csv (HasHeader(..), encode, decode)
-- >>> import Control.Newtype (Newtype(..))
-- >>> instance Show (Q [a]) where show _ = "..."

-- $use
-- Let's say we have a latitude that is a @newtype@ 'Rational' number but we
-- want it to be encoded to JSON with a fixed number of decimal places.
--
-- >>> newtype Lat = Lat Rational deriving (Eq, Ord)
--
-- Types going 'ViaSci' also need to be instances of 'DefaultDecimalPlaces' and
-- 'Newtype'.
--
-- >>> :{
-- instance DefaultDecimalPlaces Lat where
--     defdp _ = DecimalPlaces 8
-- instance Newtype Lat Rational where
--     pack = Lat
--     unpack (Lat a) = a
-- instance ToJSON Lat where
--     toJSON x = toJSON $ ViaSci x
-- instance FromJSON Lat where
--     parseJSON o = do ViaSci x <- parseJSON o; return x
-- instance Show Lat where
--     show x = show $ ViaSci x
-- :}
--
-- >>> let x = 1122334455667788 % 10000000000000000
-- >>> fromRational x
-- 0.1122334455667788
-- >>> toSci (DecimalPlaces 8) x
-- 0.11223345
--
-- When having to check numbers by hand, a fixed decimal is more familiar than
-- a ratio of possibly large integers.
--
-- >>> encode x
-- "{\"numerator\":280583613916947,\"denominator\":2500000000000000}"
-- >>> encode (Lat x)
-- "0.11223345"
--
-- With too few decimal places, the encoding will be lossy.
--
-- >>> decode (encode x) == Just x
-- True
-- >>> decode (encode (Lat x)) == Just (Lat x)
-- False
-- >>> let Just (Lat y) = decode (encode (Lat x)) in fromRational y
-- 0.11223345
--
-- Similarly for CSV.
--
-- >>> :{
-- instance ToField Lat where
--     toField = toField . ViaSci
-- instance FromField Lat where
--     parseField c = do ViaSci x <- parseField c; return x
-- :}
--
-- >>> Csv.encode [("A", Lat x)]
-- "A,0.11223345\r\n"
-- >>> Csv.decode Csv.NoHeader (Csv.encode [("B", Lat x)]) == Right (fromList [("B", Lat x)])
-- False
-- >>> Csv.decode Csv.NoHeader (Csv.encode [("C", Lat x)]) == Right (fromList [("C", Lat . fromSci . toSci (DecimalPlaces 8) $ x)])
-- True
