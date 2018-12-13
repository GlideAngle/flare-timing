module Flight.Route.TrackLine
    ( ToTrackLine(..)
    , TrackLine(..)
    ) where

import Prelude hiding (span)
import Data.List (nub)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)

import Flight.LatLng (LatLng(..))
import Flight.LatLng.Raw (RawLatLng(..))
import Flight.Distance (QTaskDistance, PathDistance(..), SpanLatLng, toKm)
import Flight.Zone (Zone(..))
import Flight.Zone.Path (distancePointToPoint)
import Flight.TaskTrack.Internal (convertLatLng, legDistances, addTaskDistance)

data TrackLine =
    TrackLine
        { distance :: Double
        , waypoints :: [RawLatLng]
        , legs :: [Double]
        , legsSum :: [Double]
        }
    deriving (Eq, Ord, Show, Generic)

instance ToJSON TrackLine
instance FromJSON TrackLine

class ToTrackLine a b where
    toTrackLine :: SpanLatLng a -> Bool -> b -> TrackLine

pathVertices
    :: (Fractional a, Real a)
    => PathDistance a
    -> ([LatLng a [u| rad |]], [RawLatLng])
pathVertices ed =
    let vs = nub $ vertices ed in (vs, convertLatLng <$> vs)

instance ToTrackLine Double (PathDistance Double) where
    toTrackLine span excludeWaypoints ed =
        TrackLine
            { distance = toKm d
            , waypoints = if excludeWaypoints then [] else xs
            , legs = toKm <$> ds
            , legsSum = toKm <$> dsSum
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
            { distance = toKm d
            , waypoints = if excludeWaypoints then [] else xs
            , legs = toKm <$> ds 
            , legsSum = toKm <$> dsSum
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
