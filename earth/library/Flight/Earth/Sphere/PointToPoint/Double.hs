{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Earth.Sphere.PointToPoint.Double (distanceHaversine) where

import Prelude hiding (sum, span)
import Data.UnitsOfMeasure (One, (-:), (*:), u)
import Data.UnitsOfMeasure.Internal (Quantity(..), mk)

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
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

