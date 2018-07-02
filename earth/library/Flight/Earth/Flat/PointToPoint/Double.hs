{-# LANGUAGE DataKinds #-}

module Flight.Earth.Flat.PointToPoint.Double
    ( distanceEuclidean
    , azimuthFwd
    , azimuthRev
    ) where

import Flight.LatLng (AzimuthFwd, AzimuthRev)
import Flight.Zone (Zone(..))
import Flight.Distance (PathDistance(..), SpanLatLng)
import Flight.Earth.Flat.Projected.Double (costEastNorth, azimuths)

-- | Euclidean distance of the projection onto a UTM plane using floating point
-- numbers.
distanceEuclidean :: (Real a, Floating a) => SpanLatLng a
distanceEuclidean x y =
    edgesSum $ costEastNorth (Point x) (Point y)

azimuthFwd :: AzimuthFwd Double
azimuthFwd x y =
    fst <$> azimuths (Point x) (Point y)

azimuthRev :: AzimuthRev Double
azimuthRev x y =
    snd <$> azimuths (Point x) (Point y)
