module Flight.Flat.PointToPoint.Double (distanceEuclidean) where

import Prelude hiding (sum, span)

import Flight.Zone (Zone(..))
import Flight.Distance (PathDistance(..), SpanLatLng)
import Flight.Flat.Projected.Double (costEastNorth)

-- | Euclidean distance of the projection onto a UTM plane using floating point
-- numbers.
distanceEuclidean :: (Real a, Floating a) => SpanLatLng a
distanceEuclidean x y =
    edgesSum $ costEastNorth (Point x) (Point y)
