{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Zone.Path (costSegment, distancePointToPoint) where

import Prelude hiding (sum, span)
import Data.List (foldl')
import Data.UnitsOfMeasure ((+:), (-:), u, abs', zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (LatLng(..))
import Flight.Zone (Zone(..), Radius(..), center)
import Flight.Distance
    (QTaskDistance, TaskDistance(..), PathDistance(..), SpanLatLng)

costSegment
    :: Real a
    => SpanLatLng a
    -> Zone a
    -> Zone a
    -> PathDistance a
costSegment span x y = distancePointToPoint span [x, y]

-- | One way of measuring task distance is going point-to-point through each
-- control zone's center along the course from start to goal. This is not used
-- by CIVL but sometimes task distance will be reported this way.
--
-- The speed section  usually goes from start exit cylinder to goal cylinder or
-- to goal line. The optimal way to fly this in a zig-zagging course will avoid
-- zone centers for a shorter flown distance.
distancePointToPoint
    :: Real a
    => SpanLatLng a
    -> [Zone a]
    -> PathDistance a
distancePointToPoint span xs =
    PathDistance
        { edgesSum = distanceViaCenters span xs
        , vertices = center <$> xs
        }
{-# INLINABLE distancePointToPoint #-}

distanceViaCenters
    :: Real a
    => SpanLatLng a
    -> [Zone a]
    -> QTaskDistance a [u| m |]

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
sum = foldl' (+:) zero

distance :: Num a => SpanLatLng a -> [Zone a] -> QTaskDistance a [u| m |]
distance span xs =
    TaskDistance $ sum $ zipWith f ys (tail ys)
    where
        ys = center <$> xs
        unwrap (TaskDistance x) = x

        f :: LatLng _ [u| rad |] -> LatLng _ [u| rad |] -> Quantity _ [u| m |]
        f = (unwrap .) . span
