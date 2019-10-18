module Flight.Mask.Internal.Zone
    ( MadeZones(..)
    , SelectedCrossings(..)
    , NomineeCrossings(..)
    , ExcludedCrossings(..)
    , ZoneEntry(..)
    , ZoneExit(..)
    , Crossing
    , TaskZone(..)
    , TrackZone(..)
    , OrdCrossing(..)
    , slice
    , fixToRadLL
    , fixToPoint
    , rowToPoint
    , fixFromFix
    , zoneToCylinder
    , zonesToTaskZones
    ) where

import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Maybe (fromMaybe)
import Data.UnitsOfMeasure (Quantity, u, fromRational', toRational')

import Flight.Units.Angle (Angle(..))
import qualified Flight.Kml as Kml
    ( Fix
    , Seconds(..)
    , Latitude(..)
    , Longitude(..)
    , Altitude(..)
    , LatLngAlt(..)
    , FixMark(..)
    )
import Flight.LatLng (AzimuthFwd, LatLng(..), degPairToRadLL)
import Flight.LatLng.Raw (RawLat(..), RawLng(..), RawAlt(..))
import Flight.Zone (Radius(..), Zone(..), realToFracZone, unlineZones)
import Flight.Zone.SpeedSection (SpeedSection, sliceZones)
import Flight.Zone.Raw (RawZone(..))
import Flight.Zone.MkZones (Zones(..), unkindZones)
import Flight.Track.Time (ZoneIdx(..), TimeRow(..))
import Flight.Track.Cross (Fix(..), ZoneCross(..), TrackFlyingSection(..))
import Flight.Units ()

data MadeZones =
    MadeZones
        { flying :: TrackFlyingSection
        , selectedCrossings :: SelectedCrossings
        , nomineeCrossings :: NomineeCrossings
        , excludedCrossings :: ExcludedCrossings
        }

-- | Crossings from the set of nominee crossings selected for possible tagging.
newtype SelectedCrossings =
    SelectedCrossings { unSelectedCrossings :: [Maybe ZoneCross] }
    deriving Show

-- | All crossings except those excluded.
newtype NomineeCrossings =
    NomineeCrossings { unNomineeCrossings :: [[Maybe ZoneCross]] }
    deriving Show

-- | Crossings excluded because they are outside of the open time window of the
-- zone.
newtype ExcludedCrossings =
    ExcludedCrossings { unExcludedCrossings :: [[Maybe ZoneCross]] }
    deriving Show

data ZoneEntry = ZoneEntry ZoneIdx ZoneIdx deriving (Eq, Show)
data ZoneExit = ZoneExit ZoneIdx ZoneIdx deriving (Eq, Show)
type Crossing = Either ZoneEntry ZoneExit

newtype OrdCrossing = OrdCrossing { unOrdCrossing :: Crossing }

pos :: OrdCrossing -> ZoneIdx
pos (OrdCrossing (Left (ZoneEntry i _))) = i
pos (OrdCrossing (Right (ZoneExit i _))) = i

instance Eq OrdCrossing where
    x == y = pos x == pos y

instance Ord OrdCrossing where
    compare x y = compare (pos x) (pos y)

instance Show OrdCrossing where
    show x = show $ pos x

-- | A task control zone.
newtype TaskZone a = TaskZone { unTaskZone :: Zone a }

-- | A fix in a flight track converted to a point zone.
newtype TrackZone a = TrackZone { unTrackZone :: Zone a }

slice :: SpeedSection -> [a] -> [a]
slice = sliceZones

fixToRadLL :: Fractional a => Fix -> LatLng a [u| rad |]
fixToRadLL Fix{lat = RawLat lat, lng = RawLng lng} =
    degPairToRadLL (lat, lng)

fixFromFix :: UTCTime -> ZoneIdx -> Kml.Fix -> Fix
fixFromFix mark0 (ZoneIdx i) x =
    -- SEE: https://ocharles.org.uk/blog/posts/2013-12-15-24-days-of-hackage-time.html
    Fix { fix = i
        , time = fromInteger secs `addUTCTime` mark0
        , lat = RawLat lat
        , lng = RawLng lng
        , alt = RawAlt $ toRational alt
        }
    where
        Kml.Seconds secs = Kml.mark x
        Kml.Latitude lat = Kml.lat x
        Kml.Longitude lng = Kml.lng x
        Kml.Altitude alt = Kml.altGps x

fixToPoint :: (Eq a, Ord a, Fractional a) => Kml.Fix -> TrackZone a
fixToPoint fix =
    TrackZone $ Point (degPairToRadLL (lat, lng))
    where
        Kml.Latitude lat = Kml.lat fix
        Kml.Longitude lng = Kml.lng fix

rowToPoint :: (Eq a, Ord a, Fractional a) => TimeRow -> TrackZone a
rowToPoint
    TimeRow{lat = RawLat lat, lng = RawLng lng} =
    TrackZone $ Point (degPairToRadLL (lat, lng))

zoneToCylinder :: (Eq a, Ord a, Fractional a) => RawZone -> Zone a
zoneToCylinder RawZone{lat = RawLat lat, lng = RawLng lng, radius, give} =
    Cylinder (Radius r') (degPairToRadLL(lat, lng))
    where
        Radius r = fromMaybe radius give
        r' = fromRational' . toRational' $ r

zonesToTaskZones
    :: (Ord a, Fractional a, Angle (Quantity a [u| rad |]))
    => AzimuthFwd a
    -> Zones
    -> [TaskZone a]

zonesToTaskZones az zs@Zones{raceKind = Just _} =
    TaskZone <$> unlineZones az (realToFracZone <$> unkindZones zs)

zonesToTaskZones az zs@Zones{openKind = Just _} =
    TaskZone <$> unlineZones az (realToFracZone <$> unkindZones zs)

zonesToTaskZones _ Zones{raw} =
    TaskZone . zoneToCylinder <$> raw
