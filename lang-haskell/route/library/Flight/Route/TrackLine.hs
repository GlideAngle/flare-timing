{-# LANGUAGE DuplicateRecordFields #-}

module Flight.Route.TrackLine
    ( ToTrackLine(..)
    , TrackLine(..)
    , ProjectedTrackLine(..)
    , PlanarTrackLine(..)
    , GeoLines(..)
    , speedSubset
    , sumLegs
    , flipSumLegs
    ) where

import Prelude hiding (span)
import Data.List (foldl')
import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u, zero)

import Flight.Field (FieldOrdering(..))
import Flight.EastNorth (EastingNorthing(..), UtmZone(..))
import Flight.LatLng (LatLng(..))
import Flight.LatLng.RawLatLng (RawLatLng(..))
import Flight.Distance
    (QTaskDistance, TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.Zone (Zone(..))
import Flight.Zone.Path (distancePointToPoint)
import Flight.Zone.SpeedSection (SpeedSection, sliceZones)
import Flight.TaskTrack.Internal (convertLatLng, legDistances, addTaskDistance, fromR)

sumLegs :: [QTaskDistance Double [u| m |]] -> [QTaskDistance Double [u| m |]]
sumLegs = scanl1 addTaskDistance

flipSumLegs :: [QTaskDistance Double [u| m |]] -> [QTaskDistance Double [u| m |]]
flipSumLegs = reverse . scanl1 addTaskDistance . reverse

data GeoLines =
    GeoLines
        { point :: TrackLine
        , sphere :: Maybe TrackLine
        , ellipse :: Maybe TrackLine
        , projected :: Maybe ProjectedTrackLine
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance FieldOrdering GeoLines where
    fieldOrder _ = cmpGeoLines

cmpGeoLines :: (Ord a, IsString a) => a -> a -> Ordering
cmpGeoLines a b =
    case (a, b) of
        ("point", _) -> LT

        ("sphere", "point") -> GT
        ("sphere", _) -> LT

        ("ellipse", "projected") -> LT
        ("ellipse", _) -> GT

        ("projected", _) -> GT

        -- EastingNorthing fields
        ("easting", _) -> LT
        ("northing", _) -> GT

        -- UtmZone fields
        ("latZone", _) -> LT
        ("lngZone", _) -> GT

        -- RawLatLng fields
        ("lat", _) -> LT
        ("lng", _) -> GT

        -- PlanarTrackLine fields
        ("distance", _) -> LT

        ("legs", "distance") -> GT
        ("legs", _) -> LT

        ("legsSum", "distance") -> GT
        ("legsSum", "legs") -> GT
        ("legsSum", _) -> LT

        ("flipSum", "distance") -> GT
        ("flipSum", "legs") -> GT
        ("flipSum", "legsSum") -> GT
        ("flipSum", _) -> LT

        ("wayPoints", _) -> GT

        _ ->  compare a b

-- | Once a track line is found on a plane, what are its equivalent latitudes
-- and longitudes on the sphere and ellipsoid?
data ProjectedTrackLine =
    ProjectedTrackLine
        { spherical :: TrackLine
        , ellipsoid :: TrackLine
        , planar :: PlanarTrackLine
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A track line on the projected plane with eastings and northings.
data PlanarTrackLine =
    PlanarTrackLine
        { distance :: QTaskDistance Double [u| m |]
        -- ^ The total distance of the track
        , mappedZones :: [UtmZone]
        , mappedPoints :: [EastingNorthing]
        , legs :: [QTaskDistance Double [u| m |]]
        -- ^ The distances between each turnpoint
        , legsSum :: [QTaskDistance Double [u| m |]]
        -- ^ The sum of leg distances from start to finish
        , flipSum :: [QTaskDistance Double [u| m |]]
        -- ^ The sum of leg distances from finish to start
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A track line on a sphere or ellipsoid with latitudes and longitudes.
data TrackLine =
    TrackLine
        { distance :: QTaskDistance Double [u| m |]
        -- ^ The total distance of the track
        , waypoints :: [RawLatLng]
        , legs :: [QTaskDistance Double [u| m |]]
        -- ^ The distances between each turnpoint
        , legsSum :: [QTaskDistance Double [u| m |]]
        -- ^ The sum of leg distances from start to finish
        , flipSum :: [QTaskDistance Double [u| m |]]
        -- ^ The sum of leg distances from finish to start
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

class ToTrackLine a b where
    toTrackLine :: SpanLatLng a -> Bool -> b -> TrackLine

speedSubset :: SpeedSection -> TrackLine -> TrackLine
speedSubset Nothing x = x
speedSubset ss TrackLine{..} =
    let legs' = sliceZones ss legs in
    TrackLine
        { distance = foldl' addTaskDistance (TaskDistance zero) legs'
        , waypoints = sliceZones ss waypoints
        , legs = legs'
        , legsSum = sumLegs legs'
        , flipSum = flipSumLegs legs'
        }

pathVertices
    :: (Fractional a, Real a)
    => PathDistance a
    -> ([LatLng a [u| rad |]], [RawLatLng])
pathVertices ed =
    -- WARNING: I used to drop duplicate vertices but this caused complications
    -- later on when rendering the task table.
    let vs = vertices ed in (vs, convertLatLng <$> vs)

instance ToTrackLine Double (PathDistance Double) where
    toTrackLine span excludeWaypoints ed =
        TrackLine
            { distance = d
            , waypoints = if excludeWaypoints then [] else xs
            , legs = ds
            , legsSum = sumLegs ds
            , flipSum = flipSumLegs ds
            }
        where
            d :: QTaskDistance Double [u| m |]
            d = edgesSum ed

            -- NOTE: The graph of points created for determining the shortest
            -- path can have duplicate points, so the shortest path too can have
            -- duplicate points. Remove these duplicates.
            --
            -- I found that by decreasing defEps, the default epsilon, used for
            -- rational math from 1/10^9 to 1/10^12 these duplicates stopped
            -- occuring.
            (vertices', xs) = pathVertices ed

            ds :: [QTaskDistance Double [u| m |]]
            ds =
                legDistances
                    distancePointToPoint
                    span
                    (Point <$> vertices' :: [Zone Double])

instance ToTrackLine Rational (PathDistance Rational) where
    toTrackLine span excludeWaypoints ed =
        TrackLine
            { distance = fromR d
            , waypoints = if excludeWaypoints then [] else xs
            , legs = fromR <$> ds
            , legsSum = sumLegs $ fromR <$> ds
            , flipSum = flipSumLegs $ fromR <$> ds
            }
        where
            d :: QTaskDistance Rational [u| m |]
            d = edgesSum ed

            -- NOTE: The graph of points created for determining the shortest
            -- path can have duplicate points, so the shortest path too can have
            -- duplicate points. Remove these duplicates.
            --
            -- I found that by decreasing defEps, the default epsilon, used for
            -- rational math from 1/10^9 to 1/10^12 these duplicates stopped
            -- occuring.
            (vertices', xs) = pathVertices ed

            ds :: [QTaskDistance Rational [u| m |]]
            ds =
                legDistances
                    distancePointToPoint
                    span
                    (Point <$> vertices' :: [Zone Rational])
