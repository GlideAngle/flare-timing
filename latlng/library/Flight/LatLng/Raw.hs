{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Flight.LatLng.Raw
    ( RawLat(..)
    , RawLng(..)
    , RawLatLng(..)
    , fromSci
    , toSci
    , showLat
    , showLng
    ) where

import Control.Newtype (Newtype(..))
import Data.Aeson
    (ToJSON(..), FromJSON(..), (.:), (.=), object, withObject)
import qualified Data.Csv as Csv ((.:))
import Data.Csv (ToNamedRecord(..), FromNamedRecord(..), namedRecord, namedField)
import Data.Aeson.Via.Scientific
    ( ViaScientific(..), DefaultDecimalPlaces(..), DecimalPlaces(..)
    , fromSci, toSci, showSci
    )

data RawLatLng =
    RawLatLng { lat :: ViaScientific RawLat
              , lng :: ViaScientific RawLng
              } deriving (Eq, Ord, Show)

instance ToJSON RawLatLng where
    toJSON RawLatLng{..} =
        object ["lat" .= lat, "lng" .= lng]

instance FromJSON RawLatLng where
    parseJSON = withObject "RawLatLng" $ \v -> RawLatLng
        <$> v .: "lat"
        <*> v .: "lng"

newtype RawLat = RawLat Rational deriving (Eq, Ord, Show)
newtype RawLng = RawLng Rational deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces RawLat where
    defdp _ = dpDegree

instance DefaultDecimalPlaces RawLng where
    defdp _ = dpDegree

instance Newtype RawLat Rational where
    pack = RawLat
    unpack (RawLat a) = a

instance Newtype RawLng Rational where
    pack = RawLng
    unpack (RawLng a) = a

-- | Decimal degrees at 8 decimal places is just a bit more than a mm.
--
--     * 1.1132 mm at the equator
--     * 1.0247 mm at 23 N/S
--     * 787.1 µm at 45 N/S
--     * 434.96 µm at 67 N/S
-- SOURCE: <https://en.wikipedia.org/wiki/Decimal_degrees>
dpDegree :: DecimalPlaces
dpDegree = DecimalPlaces 8

csvSci :: Rational -> String
csvSci = showSci dpDegree . toSci dpDegree

instance ToNamedRecord (ViaScientific RawLat) where
    toNamedRecord (ViaScientific (RawLat x)) =
        namedRecord [ namedField "lat" $ csvSci x ]

instance ToNamedRecord (ViaScientific RawLng) where
    toNamedRecord (ViaScientific (RawLng x)) =
        namedRecord [ namedField "lng" $ csvSci x ]

-- TODO: Get rid of fromDouble when upgrading to cassava-0.5.1.0
fromDouble :: Double -> Rational
fromDouble = toRational

-- TODO: Use fromSci when upgrading to cassava-0.5.1.0
instance FromNamedRecord (ViaScientific RawLat) where
    parseNamedRecord m = ViaScientific . RawLat . fromDouble <$> m Csv..: "lat"

instance FromNamedRecord (ViaScientific RawLng) where
    parseNamedRecord m = ViaScientific . RawLng . fromDouble <$> m Csv..: "lat"

showLat :: RawLat -> String
showLat (RawLat lat') =
    if x < 0
       then showSci dpDegree (negate x) ++ " S"
       else showSci dpDegree x ++ " N"
    where
        x = toSci dpDegree lat'

showLng :: RawLng -> String
showLng (RawLng lng') =
    if x < 0
       then showSci dpDegree (negate x) ++ " W"
       else showSci dpDegree x ++ " E"
    where
        x = toSci dpDegree lng'
