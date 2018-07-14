module Flight.Mask.Internal.Zone
    ( ZoneIdx
    , ZoneEntry(..)
    , ZoneExit(..)
    , Crossing
    , TaskZone(..)
    , TrackZone(..)
    , OrdCrossing(..)
    , slice
    , fixToPoint
    , rowToPoint
    , fixFromFix
    , zoneToCylinder
    ) where

import Data.Time.Clock (UTCTime, addUTCTime)
import Data.UnitsOfMeasure (u, convert, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import qualified Flight.Kml as Kml
    ( Fix
    , Seconds(..)
    , Latitude(..)
    , Longitude(..)
    , LatLngAlt(..)
    , FixMark(..)
    )
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng.Raw (RawLat(..), RawLng(..))
import Flight.Zone (Radius(..), Zone(..))
import qualified Flight.Zone.Raw as Raw (RawZone(..))
import Flight.Track.Time (TimeRow(..))
import Flight.Track.Cross (Fix(..))
import Flight.Comp (SpeedSection)
import Flight.Units ()

type ZoneIdx = Int

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

-- | Slice the speed section from a list.
slice :: SpeedSection -> [a] -> [a]
slice = \case
    Nothing -> id
    Just (s', e') -> let (s, e) = (s' - 1, e' - 1) in take (e - s + 1) . drop s

-- | The input pair is in degrees while the output is in radians.
toLL :: Fractional a => (Rational, Rational) -> LatLng a [u| rad |]
toLL (lat, lng) =
    LatLng (x', y')
    where
        lat' = MkQuantity lat :: Quantity Rational [u| deg |]
        lng' = MkQuantity lng :: Quantity Rational [u| deg |]

        (MkQuantity x) = convert lat' :: Quantity Rational [u| rad |]
        (MkQuantity y) = convert lng' :: Quantity Rational [u| rad |]

        x' = Lat . MkQuantity $ realToFrac x
        y' = Lng . MkQuantity $ realToFrac y

fixFromFix :: UTCTime -> Int -> Kml.Fix -> Fix
fixFromFix mark0 i x =
    -- SEE: https://ocharles.org.uk/blog/posts/2013-12-15-24-days-of-hackage-time.html
    Fix { fix = i
        , time = fromInteger secs `addUTCTime` mark0
        , lat = RawLat lat
        , lng = RawLng lng
        }
    where
        Kml.Seconds secs = Kml.mark x
        Kml.Latitude lat = Kml.lat x
        Kml.Longitude lng = Kml.lng x

fixToPoint :: (Eq a, Ord a, Fractional a) => Kml.Fix -> TrackZone a
fixToPoint fix =
    TrackZone $ Point (toLL (lat, lng))
    where
        Kml.Latitude lat = Kml.lat fix
        Kml.Longitude lng = Kml.lng fix

rowToPoint :: (Eq a, Ord a, Fractional a) => TimeRow -> TrackZone a
rowToPoint
    TimeRow{lat = RawLat lat, lng = RawLng lng} =
    TrackZone $ Point (toLL (lat, lng))

zoneToCylinder :: (Eq a, Ord a, Fractional a) => Raw.RawZone -> TaskZone a
zoneToCylinder z =
    TaskZone $ Cylinder (Radius r') (toLL(lat, lng))
    where
        r' = fromRational' . toRational' $ r

        Radius r = Raw.radius z
        RawLat lat = Raw.lat z
        RawLng lng = Raw.lng z
