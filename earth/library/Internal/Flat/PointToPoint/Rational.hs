module Internal.Flat.PointToPoint.Rational
    ( distance
    , azimuthFwd
    , azimuthRev
    ) where

import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units (realToFrac')
import Flight.LatLng (AzimuthFwd, AzimuthRev)
import Flight.Zone (Zone(..))
import Flight.Distance (PathDistance(..), SpanLatLng)
import Internal.Flat.Projected.Rational (costEastNorth, azimuths)

-- | Euclidean distance of the projection onto a UTM plane using rational
-- numbers.
distance :: (Real a, Fractional a) => SpanLatLng a
distance x y =
    edgesSum $ costEastNorth (Point x) (Point y)

azimuthFwd :: (Real a, Fractional a) => AzimuthFwd a
azimuthFwd x y =
    fst <$> azimuths (Point x) (Point y)

azimuthRev :: (Real a, Fractional a) => AzimuthRev a
azimuthRev x y =
    (realToFrac'
        :: (Real a, Fractional a)
        => Quantity Rational u
        -> Quantity a u)
    . snd
    <$> azimuths (Point x) (Point y)
