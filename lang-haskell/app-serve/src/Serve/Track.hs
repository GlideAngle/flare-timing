module Serve.Track
    ( RawLatLngTrack(..)
    , BolsterStats(..)
    , crossToTrack
    , tagToTrack
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..))

import Flight.Kml (MarkedFixes(..), Fix(..), LLA(..), Latitude(..), Longitude(..))
import Flight.Track.Cross (PilotTrackCross(..), TrackCross(..))
import Flight.Track.Tag (PilotTrackTag(..), TrackTag(..))
import Flight.Track.Cross (ZoneTag)
import "flight-gap-valid" Flight.Score (ReachStats)

newtype RawLatLngTrack = RawLatLngTrack MarkedFixes
    deriving (Eq, Ord, Generic)

instance ToJSON RawLatLngTrack where
    toJSON (RawLatLngTrack MarkedFixes{fixes}) =
        toJSON $ mkLatLng <$> fixes

mkLatLng :: Fix -> [Double]
mkLatLng Fix{fix = LLA{llaLat = Latitude lat', llaLng = Longitude lng'}} =
    fromRational <$> [lat', lng']

crossToTrack :: PilotTrackCross -> Maybe TrackCross
crossToTrack (PilotTrackCross _ x) = x

tagToTrack :: PilotTrackTag -> [Maybe ZoneTag]
tagToTrack (PilotTrackTag _ Nothing) = []
tagToTrack (PilotTrackTag _ (Just TrackTag{zonesTag = xs})) = xs

data BolsterStats =
    BolsterStats
        { bolster :: Maybe ReachStats
        , reach :: Maybe ReachStats
        }
    deriving (Generic, ToJSON)
