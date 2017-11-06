{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.PointToPoint.Double
    ( distancePointToPoint
    , distanceHaversine
    , costSegment
    ) where

import Prelude hiding (sum, span)
import Data.UnitsOfMeasure (One, (+:), (-:), (*:), u, abs', zero)
import Data.UnitsOfMeasure.Internal (Quantity(..), mk)

import Flight.LatLng (Lat(..), Lng(..), LatLng(..), earthRadius)
import Flight.Zone (Zone(..), Radius(..), center)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.PointToPoint.Segment (SpanLatLng)

costSegment :: Real a
            => SpanLatLng a
            -> Zone a
            -> Zone a
            -> PathDistance a
costSegment span x y = distancePointToPoint span [x, y]

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

-- | One way of measuring task distance is going point-to-point through each control
-- zone's center along the course from start to goal. This is not used by CIVL
-- but sometimes task distance will be reported this way.
--
-- The speed section  usually goes from start exit cylinder to goal cylinder
-- or to goal line. The optimal way to fly this in a zig-zagging course will
-- avoid zone centers for a shorter flown distance.
distancePointToPoint :: Real a
                     => SpanLatLng a
                     -> [Zone a]
                     -> PathDistance a
distancePointToPoint span xs =
    PathDistance
        { edgesSum = distanceViaCenters span xs
        , vertices = center <$> xs
        }

distanceViaCenters :: Real a
                   => SpanLatLng a
                   -> [Zone a]
                   -> TaskDistance a

distanceViaCenters _ [] = TaskDistance [u| 0 m |]

distanceViaCenters _ [_] = TaskDistance [u| 0 m |]

distanceViaCenters span [Cylinder (Radius xR) x, Cylinder (Radius yR) y]
    | x == y && xR /= yR = TaskDistance dR
    | otherwise = distanceViaCenters span ([Point x, Point y] :: [Zone _])
    where
        dR :: Quantity _ [u| m |]
        dR = abs' $ xR -: yR

distanceViaCenters span xs@[a, b]
    | a == b = TaskDistance zero
    | otherwise = distance span xs

distanceViaCenters span xs = distance span xs

sum :: Num a => [Quantity a [u| m |]] -> Quantity a [u| m |]
sum = foldr (+:) zero

distance :: Num a
         => SpanLatLng a
         -> [Zone a]
         -> TaskDistance a
distance span xs =
    TaskDistance $ sum $ zipWith f ys (tail ys)
    where
        ys = center <$> xs
        unwrap (TaskDistance x) = x

        f :: LatLng _ [u| rad |]
          -> LatLng _ [u| rad |]
          -> Quantity _ [u| m |]
        f = (unwrap .) . span
