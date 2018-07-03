{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Earth.Sphere.PointToPoint.Rational (distanceHaversine) where

import Data.Ratio((%))
import qualified Data.Number.FixedFunctions as F
import Data.UnitsOfMeasure (One, (-:), (*:), u, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..), mk)

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Zone (toRationalLatLng)
import Flight.Distance (TaskDistance(..), SpanLatLng)
import Flight.Earth.Sphere (earthRadius)

haversine :: Epsilon
          -> Quantity Rational [u| rad |]
          -> Quantity Rational [u| rad |]
haversine (Epsilon eps) (MkQuantity x) =
    MkQuantity $ y * y
    where
        y :: Rational
        y = F.sin eps (x * (1 % 2))

aOfHaversine :: Epsilon
  -> LatLng Rational [u| rad |]
  -> LatLng Rational [u| rad |]
  -> Rational
aOfHaversine
    e@(Epsilon eps)
    (LatLng (Lat xLat, Lng xLng))
    (LatLng (Lat yLat, Lng yLng)) =
    hLat
    + F.cos eps xLat'
    * F.cos eps yLat'
    * hLng
    where
        (dLat, dLng) = (yLat -: xLat, yLng -: xLng)
        (MkQuantity xLat') = xLat
        (MkQuantity yLat') = yLat
        (MkQuantity hLat) = haversine e dLat
        (MkQuantity hLng) = haversine e dLng

-- | Sperical distance using haversines and rational numbers.
distanceHaversine :: (Real a, Fractional a) => Epsilon -> SpanLatLng a
distanceHaversine e@(Epsilon eps) x y =
    TaskDistance . fromRational' $ radDist *: earthRadius
    where
        x' = toRationalLatLng x
        y' = toRationalLatLng y
        radDist :: Quantity Rational One
        radDist = mk $ 2 * F.asin eps (F.sqrt eps $ aOfHaversine e x' y')
