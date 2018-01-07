{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Data.Aeson.Via.Scientific
    ( ViaSci(..)
    , DecimalPlaces(..)
    , DefaultDecimalPlaces(..)
    , fromSci
    , toSci
    , showSci
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

newtype DecimalPlaces = DecimalPlaces Int

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
