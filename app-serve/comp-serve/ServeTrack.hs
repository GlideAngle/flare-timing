module ServeTrack (RawLatLngTrack(..), tagToTrack) where

import Data.Aeson (ToJSON(..))

import Flight.Kml (MarkedFixes(..), Fix(..), LLA(..), Latitude(..), Longitude(..))
import Flight.Track.Tag (PilotTrackTag(..), TrackTag(..))
import Flight.Track.Cross (ZoneTag)

newtype RawLatLngTrack = RawLatLngTrack MarkedFixes
    deriving (Eq, Ord)

instance ToJSON RawLatLngTrack where
    toJSON (RawLatLngTrack MarkedFixes{fixes}) =
        toJSON $ mkLatLng <$> fixes

mkLatLng :: Fix -> [Double]
mkLatLng Fix{fix = LLA{llaLat = Latitude lat', llaLng = Longitude lng'}} =
    fromRational <$> [lat', lng']

tagToTrack :: PilotTrackTag -> [Maybe ZoneTag]
tagToTrack (PilotTrackTag _ Nothing) = []
tagToTrack (PilotTrackTag _ (Just TrackTag{zonesTag = xs})) = xs
