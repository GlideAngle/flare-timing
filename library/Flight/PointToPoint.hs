{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flight.PointToPoint
    ( TaskDistance(..)
    , distancePointToPoint
    , distanceHaversineF
    , distanceHaversine
    , distance
    ) where

import Data.Ratio((%))
import qualified Data.Number.FixedFunctions as F

import Flight.Geo (LatLng(..), Epsilon(..), earthRadius, defEps, degToRadLL)
import Flight.Zone (Zone(..), Radius(..), center, radius)

newtype TaskDistance = TaskDistance Rational deriving (Eq, Ord, Num, Real)

instance Show TaskDistance where
    show (TaskDistance d) = "d = " ++ show (fromRational d :: Double)

diffLL :: LatLng -> LatLng -> (Rational, Rational)
diffLL (LatLng (xLat, xLng)) (LatLng (yLat, yLng)) =
    (yLat - xLat, yLng - xLng)

distanceHaversineF :: LatLng -> LatLng -> TaskDistance
distanceHaversineF xDegreeLL yDegreeLL =
    TaskDistance $ earthRadius * toRational radDist 
    where
        xLL@(LatLng (xLat, _)) = degToRadLL defEps xDegreeLL
        yLL@(LatLng (yLat, _)) = degToRadLL defEps yDegreeLL
        (dLat, dLng) = diffLL xLL yLL

        haversine :: Rational -> Double
        haversine x =
            y * y
            where
                y :: Double
                y = sin $ fromRational (x * (1 % 2))

        a :: Double
        a =
            haversine dLat
            + cos (fromRational xLat)
            * cos (fromRational yLat)
            * haversine dLng

        radDist :: Double
        radDist = 2 * asin (sqrt a)

distanceHaversine :: Epsilon -> LatLng -> LatLng -> TaskDistance
distanceHaversine (Epsilon eps) xDegreeLL yDegreeLL =
    TaskDistance $ earthRadius * radDist 
    where
        xLL@(LatLng (xLat, _)) = degToRadLL defEps xDegreeLL
        yLL@(LatLng (yLat, _)) = degToRadLL defEps yDegreeLL
        (dLat, dLng) = diffLL xLL yLL

        haversine :: Rational -> Rational
        haversine x =
            y * y
            where
                y :: Rational
                y = F.sin eps (x * (1 % 2))

        a :: Rational
        a =
            haversine dLat
            + F.cos eps xLat
            * F.cos eps yLat
            * haversine dLng

        radDist :: Rational
        radDist = 2 * F.asin eps (F.sqrt eps a)

-- | The distance from point to point less the sum of the radii of the
-- first and last points.
distancePointToPoint :: [Zone] -> TaskDistance
distancePointToPoint [] = TaskDistance 0
distancePointToPoint [_] = TaskDistance 0
distancePointToPoint xs@[a, b]
    | a == b = TaskDistance 0
    | otherwise =
        TaskDistance $ d - rx - ry
        where
            (x : _) = xs
            (Radius rx) = radius x
            (Radius ry) = radius y
            (y : _) = reverse xs
            (TaskDistance d) = distance xs
distancePointToPoint (x : xs) =
    TaskDistance $ d - rx - ry
    where
        (Radius rx) = radius x
        (Radius ry) = radius y
        (y : _) = reverse xs
        (TaskDistance d) = distance xs

distance :: [Zone] -> TaskDistance
distance xs =
    TaskDistance $ sum $ zipWith f ys (tail ys)
    where
        ys = center <$> xs
        unwrap (TaskDistance x) = x
        f = (unwrap .) . distanceHaversine defEps
