module Internal.Flat.PointToPoint.Double
    ( distance
    , azimuthFwd
    , azimuthRev
    ) where

import Flight.LatLng (AzimuthFwd, AzimuthRev)
import Flight.Zone (Zone(..))
import Flight.Distance (PathDistance(..), SpanLatLng)
import Internal.Flat.Projected.Double (costEastNorth, azimuths)

-- | Euclidean distance of the projection onto a UTM plane using floating point
-- numbers.
distance :: (Real a, Fractional a) => SpanLatLng a
distance x y =
    edgesSum $ costEastNorth (Point x) (Point y)

azimuthFwd :: (Real a, Fractional a) => AzimuthFwd a
azimuthFwd x y =
    fst <$> azimuths (Point x) (Point y)

azimuthRev :: (Real a, Fractional a) => AzimuthRev a
azimuthRev x y =
    snd <$> azimuths (Point x) (Point y)
