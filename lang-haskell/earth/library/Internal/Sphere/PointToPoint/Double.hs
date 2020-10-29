{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Internal.Sphere.PointToPoint.Double
    ( distance
    , azimuthFwd
    , azimuthRev
    ) where

import Prelude hiding (flip)
import Data.UnitsOfMeasure (One, (-:), (*:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..), mk)

import Flight.Units (realToFrac')
import Flight.Units.Angle (Angle(..))
import Flight.LatLng (Lat(..), Lng(..), LatLng(..), AzimuthFwd, AzimuthRev)
import Flight.Distance (TaskDistance(..), SpanLatLng)
import Flight.Zone (Radius(..), realToFracLatLng)
import Flight.Earth.Sphere (earthRadius)

haversine :: Floating a
          => Quantity a [u| rad |]
          -> Quantity a [u| rad |]
haversine (MkQuantity x) =
    MkQuantity $ y * y
    where
        y = sin (x / 2)
{-# INLINABLE haversine #-}
{-# SPECIALIZE
   haversine
       :: Quantity Double [u| rad |]
       -> Quantity Double [u| rad |] #-}

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
{-# INLINABLE aOfHaversine #-}
{-# SPECIALIZE
    aOfHaversine
      :: LatLng Double [u| rad |]
      -> LatLng Double [u| rad |]
      -> Double #-}

-- | Spherical distance using haversines and floating point numbers.
distance :: Floating a => SpanLatLng a
distance x y =
    TaskDistance $ radDist *: rEarth
    where
        Radius rEarth = earthRadius
        radDist :: Quantity _ One
        radDist = mk $ 2 * asin (sqrt $ aOfHaversine x y)
{-# INLINABLE distance #-}
{-# SPECIALIZE distance :: SpanLatLng Double #-}

azimuthFwd :: (Real a, Fractional a) => AzimuthFwd a
azimuthFwd x y = do
    let x' = realToFracLatLng x
    let y' = realToFracLatLng y
    realToFrac' <$> azimuthFwd' x' y'
{-# INLINABLE azimuthFwd #-}
{-# SPECIALIZE azimuthFwd :: AzimuthFwd Double #-}

azimuthRev :: (Real a, Fractional a) => AzimuthRev a
azimuthRev x y = do
    let x' = realToFracLatLng x
    let y' = realToFracLatLng y
    realToFrac' <$> azimuthRev' x' y'
{-# INLINABLE azimuthRev #-}
{-# SPECIALIZE azimuthRev :: AzimuthRev Double #-}

-- SEE: https://www.movable-type.co.uk/scripts/latlong.html
azimuthFwd' :: AzimuthFwd Double
azimuthFwd' (LatLng (Lat xLatF, Lng xLngF)) (LatLng (Lat yLatF, Lng yLngF)) =
    Just . MkQuantity $ atan2 x y
    where
        MkQuantity xLatF' = xLatF
        MkQuantity yLatF' = yLatF
        MkQuantity xLngF' = xLngF
        MkQuantity yLngF' = yLngF

        deltaLng = yLngF' - xLngF'
        x = sin deltaLng * cos yLatF'
        y = cos xLatF' * sin yLatF' - sin xLatF' * cos yLatF' * cos deltaLng
{-# INLINE azimuthFwd' #-}

azimuthRev' :: AzimuthRev Double
azimuthRev' x y =
    rotate flip <$> azimuthFwd y x
    where
        flip :: Quantity _ [u| rad |]
        flip = convert [u| 180 deg |]
{-# INLINE azimuthRev' #-}
