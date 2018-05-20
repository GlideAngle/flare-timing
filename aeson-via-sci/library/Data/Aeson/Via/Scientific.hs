{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

module Data.Aeson.Via.Scientific
    ( ViaSci(..)
    , DecimalPlaces(..)
    , DefaultDecimalPlaces(..)
    , fromSci
    , toSci
    , showSci
    , dpDegree
    , deriveDefDec
    , deriveConstDec
    , deriveViaSci
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

-- | Decimal degrees at 8 decimal places is just a bit more than a mm.
--
--     * 1.1132 mm at the equator
--     * 1.0247 mm at 23 N/S
--     * 787.1 µm at 45 N/S
--     * 434.96 µm at 67 N/S
-- SOURCE: <https://en.wikipedia.org/wiki/Decimal_degrees>
dpDegree :: DecimalPlaces
dpDegree = DecimalPlaces 8

-- NOTE: For deriving Lift, see https://ghc.haskell.org/trac/ghc/ticket/14296
newtype DecimalPlaces = DecimalPlaces Int deriving Lift

fromSci :: Scientific -> Rational
fromSci x = toRational (toRealFloat x :: Double)

toSci :: DecimalPlaces -> Rational -> Scientific
toSci (DecimalPlaces dp) x =
    case fromRationalRepetend (Just $ dp + 1) x of
        Left (s, _) -> s
        Right (s, _) -> s

showSci :: DecimalPlaces -> Scientific -> String
showSci (DecimalPlaces dp) =
    formatScientific Fixed (Just dp)

class DefaultDecimalPlaces a where
    defdp :: a -> DecimalPlaces
    defdp _ = DecimalPlaces 0

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
deriveDefDec :: Int -> Name -> Q [Dec]
deriveDefDec dp name =
    [d|
        instance DefaultDecimalPlaces $(conT name) where
            defdp _ = DecimalPlaces dp
        |]

deriveConstDec :: DecimalPlaces -> Name -> Q [Dec]
deriveConstDec dp name =
    [d|
        instance DefaultDecimalPlaces $(conT name) where
            defdp _ = $(lift dp)
        |]

deriveViaSci :: Name -> Q [Dec]
deriveViaSci name =
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
