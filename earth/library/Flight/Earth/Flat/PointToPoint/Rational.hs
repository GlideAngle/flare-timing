module Flight.Earth.Flat.PointToPoint.Rational (distanceEuclidean) where

import Prelude hiding (sum, span)

import Flight.Zone (Zone(..))
import Flight.Distance (PathDistance(..), SpanLatLng)
import Flight.Earth.Flat.Projected.Rational (costEastNorth)

-- | Euclidean distance of the projection onto a UTM plane using rational
-- numbers.
distanceEuclidean :: SpanLatLng Rational
distanceEuclidean x y =
    edgesSum $ costEastNorth (Point x) (Point y)
