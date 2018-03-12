{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Earth.Sphere.PointToPoint.Double
    ( distanceHaversine
    , azimuthFwd
    , azimuthRev
    ) where

import Prelude hiding (flip, sum, span)
import Data.UnitsOfMeasure (One, (-:), (*:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..), mk)

import Flight.Units ()
import Flight.Units.Angle (Angle(..))
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng (AzimuthFwd, AzimuthRev)
import Flight.Distance (TaskDistance(..), SpanLatLng)
import Flight.Earth.Sphere (earthRadius)

haversine :: Floating a
          => Quantity a [u| rad |]
          -> Quantity a [u| rad |]
haversine (MkQuantity x) =
    MkQuantity $ y * y
    where
        y = sin (x / 2)

aOfHaversine :: Floating a
  => LatLng a [u| rad |]
  -> LatLng a [u| rad |]
  -> a
aOfHaversine (LatLng (Lat xLatF, Lng xLngF)) (LatLng (Lat yLatF, Lng yLngF)) =
    hLatF
    + cos xLatF'
    * cos yLatF'
    * hLngF
    where
        -- NOTE: Use xLatF etc to avoid an hlint duplication warning.
        (MkQuantity xLatF') = xLatF
        (MkQuantity yLatF') = yLatF
        (MkQuantity hLatF) = haversine dLatF
        (MkQuantity hLngF) = haversine dLngF
        (dLatF, dLngF) = (yLatF -: xLatF, yLngF -: xLngF)

-- | Sperical distance using haversines and floating point numbers.
distanceHaversine :: Floating a => SpanLatLng a
distanceHaversine x y =
    TaskDistance $ radDist *: earthRadius
    where
        radDist :: Quantity _ One
        radDist = mk $ 2 * asin (sqrt $ aOfHaversine x y)

-- SEE: https://www.movable-type.co.uk/scripts/latlong.html
azimuthFwd :: AzimuthFwd Double
azimuthFwd (LatLng (Lat xLatF, Lng xLngF)) (LatLng (Lat yLatF, Lng yLngF)) =
    Just . MkQuantity $ atan2 x y
    where
        MkQuantity xLatF' = xLatF
        MkQuantity yLatF' = yLatF
        MkQuantity xLngF' = xLngF
        MkQuantity yLngF' = yLngF

        deltaLng = yLngF' - xLngF'
        x = sin deltaLng * cos yLatF'
        y = cos xLatF' * sin yLatF' - sin xLatF' * cos yLatF' * cos deltaLng

azimuthRev :: AzimuthRev Double
azimuthRev x y =
    rotate flip <$> azimuthFwd y x
    where
        flip :: Quantity _ [u| rad |]
        flip = convert [u| 180 deg |]
