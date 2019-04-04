module Flight.Earth.Flat.PointToPoint.Rational
    ( distanceEuclidean
    , azimuthFwd
    , azimuthRev
    ) where

import Flight.LatLng (AzimuthFwd, AzimuthRev)
import Flight.Zone (Zone(..))
import Flight.Distance (PathDistance(..), SpanLatLng)
import Flight.Earth.Flat.Projected.Rational (costEastNorth, azimuths)

-- | Euclidean distance of the projection onto a UTM plane using rational
-- numbers.
distanceEuclidean :: SpanLatLng Rational
distanceEuclidean x y =
    edgesSum $ costEastNorth (Point x) (Point y)

azimuthFwd :: AzimuthFwd Rational
azimuthFwd x y =
    fst <$> azimuths (Point x) (Point y)

azimuthRev :: AzimuthRev Rational
azimuthRev x y =
    snd <$> azimuths (Point x) (Point y)
