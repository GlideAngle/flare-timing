{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Earth.Sphere.PointToPoint.Rational
    ( distanceHaversine
    , azimuthFwd
    , azimuthRev
    ) where

import Prelude hiding (flip)
import Data.Ratio((%))
import qualified Data.Number.FixedFunctions as F
import Data.UnitsOfMeasure (One, (-:), (*:), u, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..), mk)

import Flight.Units ()
import Flight.Units.Angle (Angle(..))
import Flight.LatLng (Lat(..), Lng(..), LatLng(..), AzimuthFwd, AzimuthRev)
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Zone (toRationalLatLng)
import Flight.Distance (TaskDistance(..), SpanLatLng)
import Flight.Zone (Radius(..))
import Flight.Earth.Sphere (earthRadius)
import Flight.Earth.Math

haversine
    :: Epsilon
    -> Quantity Rational [u| rad |]
    -> Quantity Rational [u| rad |]
haversine (Epsilon eps) (MkQuantity x) =
    MkQuantity $ y * y
    where
        y :: Rational
        y = F.sin eps (x * (1 % 2))

aOfHaversine
    :: Epsilon
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

-- | Spherical distance using haversines and rational numbers.
distanceHaversine :: (Real a, Fractional a) => Epsilon -> SpanLatLng a
distanceHaversine e@(Epsilon eps) x y =
    TaskDistance . fromRational' $ radDist *: rEarth
    where
        Radius rEarth = earthRadius
        x' = toRationalLatLng x
        y' = toRationalLatLng y
        radDist :: Quantity Rational One
        radDist = mk $ 2 * F.asin eps (F.sqrt eps $ aOfHaversine e x' y')

-- SEE: https://www.movable-type.co.uk/scripts/latlong.html
azimuthFwd :: Epsilon -> AzimuthFwd Rational
azimuthFwd
    e@(Epsilon eps)
    (LatLng (Lat xLatF, Lng xLngF))
    (LatLng (Lat yLatF, Lng yLngF)) =
    Just . MkQuantity $ atan2' e x y
    where
        MkQuantity xLatF' = xLatF
        MkQuantity yLatF' = yLatF
        MkQuantity xLngF' = xLngF
        MkQuantity yLngF' = yLngF

        deltaLng = yLngF' - xLngF'
        x = sin' deltaLng * cos' yLatF'
        y = cos' xLatF' * sin' yLatF' - sin' xLatF' * cos' yLatF' * cos' deltaLng

        sin' = F.sin eps
        cos' = F.cos eps

azimuthRev :: Epsilon -> AzimuthRev Rational
azimuthRev e x y =
    rotate flip <$> azimuthFwd e y x
    where
        flip :: Quantity _ [u| rad |]
        flip = convert [u| 180 deg |]
