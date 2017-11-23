{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Flight.LatLng.Raw
    ( RawLat(..)
    , RawLng(..)
    , RawLatLng(..)
    , fromSci
    , toSci
    , showLat
    , showLng
    ) where

import GHC.Generics (Generic)
import Control.Applicative (empty)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(Number))
import Data.Csv
    ( ToNamedRecord(..)
    , FromNamedRecord(..)
    , FromField(..)
    , (.:)
    , namedRecord
    , namedField
    )
import Data.Scientific
    ( Scientific
    , FPFormat(..)
    , toRealFloat
    , fromRationalRepetend
    , formatScientific
    )

data RawLatLng =
    RawLatLng { lat :: RawLat
              , lng :: RawLng
              } deriving (Eq, Show, Generic)

instance ToJSON RawLatLng
instance FromJSON RawLatLng

newtype RawLat = RawLat Rational deriving (Eq, Show)
newtype RawLng = RawLng Rational deriving (Eq, Show)

-- | Decimal degrees at 8 decimal places is just a bit more than a mm.
--
--     * 1.1132 mm at the equator
--     * 1.0247 mm at 23 N/S
--     * 787.1 µm at 45 N/S
--     * 434.96 µm at 67 N/S
-- SOURCE: <https://en.wikipedia.org/wiki/Decimal_degrees>
dpDegree :: Int
dpDegree = 8

fromSci :: Scientific -> Rational
fromSci x = toRational (toRealFloat x :: Double)

toSci :: Rational -> Scientific
toSci x =
    case fromRationalRepetend (Just $ dpDegree + 1) x of
        Left (s, _) -> s
        Right (s, _) -> s

showSci :: Scientific -> String
showSci =
    formatScientific Fixed (Just dpDegree)

csvSci :: Rational -> String
csvSci = showSci . toSci

instance ToNamedRecord RawLat where
    toNamedRecord (RawLat x) =
        namedRecord [ namedField "lat" $ csvSci x ]

instance ToNamedRecord RawLng where
    toNamedRecord (RawLng x) =
        namedRecord [ namedField "lng" $ csvSci x ]

-- TODO: Get rid of fromDouble when upgrading to cassava-0.5.1.0
fromDouble :: Double -> Rational
fromDouble = toRational

-- TODO: Use fromSci when upgrading to cassava-0.5.1.0
instance FromNamedRecord RawLat where
    parseNamedRecord m = RawLat . fromDouble <$> m .: "lat"

instance FromNamedRecord RawLng where
    parseNamedRecord m = RawLng . fromDouble <$> m .: "lat"

instance FromField RawLat where
    parseField m = RawLat . fromDouble <$> parseField m

instance FromField RawLng where
    parseField m = RawLng . fromDouble <$> parseField m

instance ToJSON RawLat where
    toJSON (RawLat x) = Number $ toSci x

instance FromJSON RawLat where
    parseJSON x@(Number _) = RawLat . fromSci <$> parseJSON x
    parseJSON _ = empty

instance ToJSON RawLng where
    toJSON (RawLng x) = Number $ toSci x

instance FromJSON RawLng where
    parseJSON x@(Number _) = RawLng . fromSci <$> parseJSON x
    parseJSON _ = empty

showLat :: RawLat -> String
showLat (RawLat lat') =
    if x < 0
       then showSci (negate x) ++ " S"
       else showSci x ++ " N"
    where
        x = toSci lat'

showLng :: RawLng -> String
showLng (RawLng lng') =
    if x < 0
       then showSci (negate x) ++ " W"
       else showSci x ++ " E"
    where
        x = toSci lng'
