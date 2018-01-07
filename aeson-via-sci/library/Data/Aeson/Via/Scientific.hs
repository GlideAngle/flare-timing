{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Data.Aeson.Via.Scientific
    ( ViaScientific(..)
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

data ViaScientific n where
    ViaScientific :: (DefaultDecimalPlaces n, Newtype n Rational)
                  => n 
                  -> ViaScientific n

deriving instance (Eq n) => Eq (ViaScientific n)
deriving instance (Ord n) => Ord (ViaScientific n)
deriving instance (Show n) => Show (ViaScientific n)

instance (DefaultDecimalPlaces n, Newtype n Rational)
         => ToJSON (ViaScientific n) where
    toJSON (ViaScientific x) = Number $ toSci (defdp x) (unpack x)

instance (DefaultDecimalPlaces n, Newtype n Rational)
         => FromJSON (ViaScientific n) where
    parseJSON x@(Number _) =
        ViaScientific <$> (pack . fromSci <$> parseJSON x)
    parseJSON _ = empty

instance (DefaultDecimalPlaces n, Newtype n Rational)
         => ToField (ViaScientific n) where
    toField (ViaScientific x) = toField $ toSci (defdp x) (unpack x)

instance (DefaultDecimalPlaces n, Newtype n Rational)
         => FromField (ViaScientific n) where
    parseField x = ViaScientific <$> (pack . fromSci <$> parseField x)
