{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Sphere.PointToPoint.Rational
    ( distancePointToPoint
    , distanceHaversine
    , costSegment
    ) where

import Prelude hiding (sum, span)
import Data.Ratio((%))
import qualified Data.Number.FixedFunctions as F
import Data.UnitsOfMeasure (One, (+:), (-:), (*:), u, abs', zero, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..), mk)

import Flight.LatLng (Lat(..), Lng(..), LatLng(..), earthRadius)
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Zone
    ( Zone(..)
    , Radius(..)
    , center
    , toRationalLatLng
    , fromRationalLatLng
    , toRationalZone
    )
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)

costSegment :: (Real a, Fractional a)
            => SpanLatLng a
            -> Zone a
            -> Zone a
            -> PathDistance a 
costSegment span x y =
    distancePointToPoint span [x, y]

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

-- | One way of measuring task distance is going point-to-point through each control
-- zone's center along the course from start to goal. This is not used by CIVL
-- but sometimes task distance will be reported this way.
--
-- The speed section  usually goes from start exit cylinder to goal cylinder
-- or to goal line. The optimal way to fly this in a zig-zagging course will
-- avoid zone centers for a shorter flown distance.
distancePointToPoint :: (Real a, Fractional a)
                     => SpanLatLng a
                     -> [Zone a]
                     -> PathDistance a
distancePointToPoint span xs =
    PathDistance
        { edgesSum = distanceViaCenters span xs
        , vertices = center <$> xs
        }

distanceViaCenters :: (Real a, Fractional a)
                   => SpanLatLng a
                   -> [Zone a]
                   -> TaskDistance a

distanceViaCenters _ [] = TaskDistance [u| 0 m |]

distanceViaCenters _ [_] = TaskDistance [u| 0 m |]

distanceViaCenters span [Cylinder (Radius xR) x, Cylinder (Radius yR) y]
    | x == y && xR /= yR = TaskDistance $ fromRational' dR
    | otherwise = distanceViaCenters span ([Point x, Point y] :: [Zone _])
    where
        xR' = toRational' xR
        yR' = toRational' yR

        dR :: Quantity Rational [u| m |]
        dR = abs' $ xR' -: yR'

distanceViaCenters span xs@[a, b]
    | a == b = TaskDistance zero
    | otherwise = distance span xs

distanceViaCenters span xs = distance span xs

sum :: Num a => [Quantity a [u| m |]] -> Quantity a [u| m |]
sum = foldr (+:) zero

distance :: (Real a, Fractional a)
         => SpanLatLng a
         -> [Zone a]
         -> TaskDistance a
distance span xs =
    TaskDistance $ fromRational' d
    where
        ys :: [LatLng Rational [u| rad |]]
        ys = center . toRationalZone <$> xs

        unwrap (TaskDistance x) = x

        f :: LatLng Rational [u| rad |]
          -> LatLng Rational [u| rad |]
          -> Quantity Rational [u| m |]
        f = (unwrap .) . span'

        d :: Quantity Rational [u| m |]
        d = sum $ zipWith f ys (tail ys)

        span' :: SpanLatLng Rational
        span' x y =
            TaskDistance $ toRational' sd
            where
                TaskDistance sd =
                    span (fromRationalLatLng x) (fromRationalLatLng y)
