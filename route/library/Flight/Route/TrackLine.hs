{-# LANGUAGE DuplicateRecordFields #-}

module Flight.Route.TrackLine
    ( ToTrackLine(..)
    , TrackLine(..)
    , ProjectedTrackLine(..)
    , PlanarTrackLine(..)
    , GeoLines(..)
    , speedSubset
    ) where

import Prelude hiding (span)
import Data.List (nub, foldl')
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u, zero)

import Flight.EastNorth (EastingNorthing(..), UtmZone(..))
import Flight.LatLng (LatLng(..))
import Flight.LatLng.Raw (RawLatLng(..))
import Flight.Distance
    (QTaskDistance, TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.Zone (Zone(..))
import Flight.Zone.Path (distancePointToPoint)
import Flight.Zone.MkZones (SpeedSection, sliceZones)
import Flight.TaskTrack.Internal (convertLatLng, legDistances, addTaskDistance, fromR)

data GeoLines =
    GeoLines
        { point :: TrackLine
        , sphere :: Maybe TrackLine
        , ellipse :: Maybe TrackLine
        , projected :: Maybe ProjectedTrackLine
        }

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
        , mappedZones :: [UtmZone]
        , mappedPoints :: [EastingNorthing]
        , legs :: [QTaskDistance Double [u| m |]]
        , legsSum :: [QTaskDistance Double [u| m |]]
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A track line on a sphere or ellipsoid with latitudes and longitudes.
data TrackLine =
    TrackLine
        { distance :: QTaskDistance Double [u| m |]
        , waypoints :: [RawLatLng]
        , legs :: [QTaskDistance Double [u| m |]]
        , legsSum :: [QTaskDistance Double [u| m |]]
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
        , legsSum = scanl1 addTaskDistance legs'
        }

pathVertices
    :: (Fractional a, Real a)
    => PathDistance a
    -> ([LatLng a [u| rad |]], [RawLatLng])
pathVertices ed =
    let vs = nub $ vertices ed in (vs, convertLatLng <$> vs)

instance ToTrackLine Double (PathDistance Double) where
    toTrackLine span excludeWaypoints ed =
        TrackLine
            { distance = d
            , waypoints = if excludeWaypoints then [] else xs
            , legs = ds
            , legsSum = dsSum
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

            dsSum :: [QTaskDistance Double [u| m |]]
            dsSum = scanl1 addTaskDistance ds

instance ToTrackLine Rational (PathDistance Rational) where
    toTrackLine span excludeWaypoints ed =
        TrackLine
            { distance = fromR d
            , waypoints = if excludeWaypoints then [] else xs
            , legs = fromR <$> ds
            , legsSum = fromR <$> dsSum
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

            dsSum :: [QTaskDistance Rational [u| m |]]
            dsSum = scanl1 addTaskDistance ds
