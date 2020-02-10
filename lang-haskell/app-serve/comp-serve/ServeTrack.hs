module ServeTrack (RawLatLngTrack(..), BolsterStats(..), tagToTrack) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..))

import Flight.Kml (MarkedFixes(..), Fix(..), LLA(..), Latitude(..), Longitude(..))
import Flight.Track.Tag (PilotTrackTag(..), TrackTag(..))
import Flight.Track.Cross (ZoneTag)
import Flight.Score (ReachStats)

newtype RawLatLngTrack = RawLatLngTrack MarkedFixes
    deriving (Eq, Ord, Generic)

instance ToJSON RawLatLngTrack where
    toJSON (RawLatLngTrack MarkedFixes{fixes}) =
        toJSON $ mkLatLng <$> fixes

mkLatLng :: Fix -> [Double]
mkLatLng Fix{fix = LLA{llaLat = Latitude lat', llaLng = Longitude lng'}} =
    fromRational <$> [lat', lng']

tagToTrack :: PilotTrackTag -> [Maybe ZoneTag]
tagToTrack (PilotTrackTag _ Nothing) = []
tagToTrack (PilotTrackTag _ (Just TrackTag{zonesTag = xs})) = xs

data BolsterStats =
    BolsterStats
        { bolster :: ReachStats
        , reach :: ReachStats
        }
    deriving (Generic, ToJSON)
