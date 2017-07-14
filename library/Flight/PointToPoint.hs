{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flight.PointToPoint
    ( TaskDistance(..)
    , distancePointToPoint
    , distanceHaversineF
    , distanceHaversine
    ) where

import Data.Ratio((%))
import qualified Data.Number.FixedFunctions as F

import Flight.Geo (LatLng(..), Epsilon(..), earthRadius, defEps, degToRadLL)
import Flight.Zone (Zone(..), Radius(..), center)

newtype TaskDistance = TaskDistance Rational deriving (Eq, Ord, Num, Real)

instance Show TaskDistance where
    show (TaskDistance d) = "d = " ++ show (fromRational d :: Double)

-- | Sperical distance using haversines and floating point numbers.
distanceHaversineF :: LatLng -> LatLng -> TaskDistance
distanceHaversineF xDegreeLL yDegreeLL =
    TaskDistance $ earthRadius * toRational radDist 
    where
        -- NOTE: Use xLatF etc to avoid an hlint duplication warning.
        LatLng (xLatF, xLngF) = degToRadLL defEps xDegreeLL
        LatLng (yLatF, yLngF) = degToRadLL defEps yDegreeLL
        (dLatF, dLngF) = (yLatF - xLatF, yLngF - xLngF)

        haversine :: Rational -> Double
        haversine x =
            y * y
            where
                y :: Double
                y = sin $ fromRational (x * (1 % 2))

        a :: Double
        a =
            haversine dLatF
            + cos (fromRational xLatF)
            * cos (fromRational yLatF)
            * haversine dLngF

        radDist :: Double
        radDist = 2 * asin (sqrt a)

-- | Sperical distance using haversines and rational numbers.
distanceHaversine :: Epsilon -> LatLng -> LatLng -> TaskDistance
distanceHaversine (Epsilon eps) xDegreeLL yDegreeLL =
    TaskDistance $ earthRadius * radDist 
    where
        LatLng (xLat, xLng) = degToRadLL defEps xDegreeLL
        LatLng (yLat, yLng) = degToRadLL defEps yDegreeLL
        (dLat, dLng) = (yLat - xLat, yLng - xLng)

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

-- | One way of measuring task distance is going point-to-point through each control
-- zone's center along the course from start to goal. This is not used by CIVL
-- but sometimes task distance will be reported this way.
--
-- The speed section  usually goes from start exit cylinder to goal cylinder
-- or to goal line. The optimal way to fly this in a zig-zagging course will
-- avoid zone centers for a shorter flown distance.
distancePointToPoint :: [Zone] -> TaskDistance

distancePointToPoint [] = TaskDistance 0

distancePointToPoint [_] = TaskDistance 0

distancePointToPoint [Cylinder (Radius xR) x, Cylinder (Radius yR) y]
    | x == y && xR /= yR = TaskDistance dR
    | otherwise = distancePointToPoint [Point x, Point y]
    where
        dR = abs $ xR - yR

distancePointToPoint xs@[a, b]
    | a == b = TaskDistance 0
    | otherwise = distance xs

distancePointToPoint xs = distance xs


distance :: [Zone] -> TaskDistance
distance xs =
    TaskDistance $ sum $ zipWith f ys (tail ys)
    where
        ys = center <$> xs
        unwrap (TaskDistance x) = x
        f = (unwrap .) . distanceHaversine defEps
