module ServeTrack (RawLatLngTrack(..)) where

import Data.Aeson (ToJSON(..))

import Flight.Kml (MarkedFixes(..), Fix(..), LLA(..), Latitude(..), Longitude(..))

newtype RawLatLngTrack = RawLatLngTrack MarkedFixes
    deriving (Eq, Ord)

instance ToJSON RawLatLngTrack where
    toJSON (RawLatLngTrack MarkedFixes{fixes}) =
        toJSON $ mkLatLng <$> fixes

mkLatLng :: Fix -> [Double]
mkLatLng Fix{fix = LLA{llaLat = Latitude lat', llaLng = Longitude lng'}} =
    fromRational <$> [lat', lng']
