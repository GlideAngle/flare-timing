module Flight.Mask.Internal.Zone
    ( MadeZones(..)
    , SelectedCrossings(..)
    , NomineeCrossings(..)
    , SelectedStart(..)
    , NomineeStarts(..)
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
    , zonesToTaskZones
    , boundingZone
    ) where

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.UnitsOfMeasure (Quantity, u)

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
import Flight.Zone (Zone(..), realToFracZone, unlineZones)
import Flight.Zone.SpeedSection (SpeedSection, sliceZones)
import Flight.Zone.Raw (Give)
import Flight.Zone.MkZones (Zones(..), unkindZones, inflate, deflate)
import Flight.Track.Time (ZoneIdx(..), TimeRow(..))
import Flight.Track.Cross (Fix(..), ZoneCross(..), TrackFlyingSection(..))
import Flight.Units ()
import Flight.Comp (StartGate(..))

data MadeZones =
    MadeZones
        { flying :: TrackFlyingSection
        , selectedCrossings :: SelectedCrossings
        , nomineeCrossings :: NomineeCrossings
        , selectedStart :: SelectedStart
        , nomineeStarts :: NomineeStarts
        , excludedCrossings :: ExcludedCrossings
        }
    deriving (Generic, NFData)

-- | Crossings from the set of nominee crossings selected for possible tagging.
newtype SelectedCrossings =
    SelectedCrossings { unSelectedCrossings :: [Maybe ZoneCross] }
    deriving (Show, Generic)
    deriving anyclass NFData

-- | All crossings except those excluded.
newtype NomineeCrossings =
    NomineeCrossings { unNomineeCrossings :: [[Maybe ZoneCross]] }
    deriving (Show, Generic)
    deriving anyclass NFData

-- | The start gate selected with its crossing.
newtype SelectedStart =
    SelectedStart { unSelectedStart :: Maybe (StartGate, ZoneCross) }
    deriving (Show, Generic)
    deriving anyclass NFData

-- | Crossings of the start zone, partitioned for each start gate.
newtype NomineeStarts =
    NomineeStarts { unNomineeStarts :: [(StartGate, [ZoneCross])] }
    deriving (Show, Generic)
    deriving anyclass NFData

-- | Crossings excluded because they are outside of the open time window of the
-- zone.
newtype ExcludedCrossings =
    ExcludedCrossings { unExcludedCrossings :: [[Maybe ZoneCross]] }
    deriving (Show, Generic)
    deriving anyclass NFData

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
data TaskZone a
    = SharpZone { sharpZone :: Zone a }
    | BluntZone { innerZone :: Zone a, outerZone :: Zone a }

-- | A fix in a flight track converted to a point zone.
newtype TrackZone a = TrackZone { unTrackZone :: Zone a }

boundingZone :: TaskZone a -> Zone a
boundingZone = \case
    SharpZone z -> z
    BluntZone _ z -> z

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
{-# INLINABLE fixToPoint #-}

rowToPoint :: (Eq a, Ord a, Fractional a) => TimeRow -> TrackZone a
rowToPoint
    TimeRow{lat = RawLat lat, lng = RawLng lng} =
    TrackZone $ Point (degPairToRadLL (lat, lng))

zonesToTaskZones
    :: (Ord a, Fractional a, Angle (Quantity a [u| rad |]))
    => Maybe Give
    -> AzimuthFwd a
    -> Zones
    -> [TaskZone a]

zonesToTaskZones give az zs@Zones{raceKind = Just _} =
    BluntZone
    <$> unlineZones az (realToFracZone <$> unkindZones inflate give zs)
    <*> unlineZones az (realToFracZone <$> unkindZones deflate give zs)

zonesToTaskZones give az zs@Zones{openKind = Just _} =
    BluntZone
    <$> unlineZones az (realToFracZone <$> unkindZones inflate give zs)
    <*> unlineZones az (realToFracZone <$> unkindZones deflate give zs)

zonesToTaskZones give _ zs =
    BluntZone
    <$> (realToFracZone <$> unkindZones inflate give zs)
    <*> (realToFracZone <$> unkindZones deflate give zs)
