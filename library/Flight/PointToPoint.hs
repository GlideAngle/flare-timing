{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Flight.PointToPoint
    ( TaskDistance(..)
    , distancePointToPoint
    , distanceHaversineF
    , distanceHaversine
    ) where

import Data.Ratio((%))
import qualified Data.Number.FixedFunctions as F

import Flight.Geo (LatLng(..), Epsilon(..), earthRadius, defEps)
import Flight.Zone (Zone(..))

newtype TaskDistance = TaskDistance Rational deriving (Eq, Ord, Num, Real)

instance Show TaskDistance where
    show (TaskDistance d) =
        "TaskDistance " ++ show (fromRational d :: Double)

center :: Zone -> LatLng
center (Point x) = x
center (Vector _ x) = x
center (Cylinder _ x) = x
center (Conical _ _ x) = x
center (Line _ x) = x
center (SemiCircle _ x) = x

diffLL :: LatLng -> LatLng -> (Rational, Rational)
diffLL (LatLng (xLat, xLng)) (LatLng (yLat, yLng)) =
    (yLat - xLat, yLng - xLng)

distanceHaversineF :: LatLng -> LatLng -> TaskDistance
distanceHaversineF xLL@(LatLng (xLat, _)) yLL@(LatLng (yLat, _)) =
    TaskDistance $ earthRadius * toRational radDist 
    where
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
        radDist = 2 * atan2 (sqrt a) (sqrt $ 1 - a)

distanceHaversine :: Epsilon -> LatLng -> LatLng -> TaskDistance
distanceHaversine (Epsilon eps) xLL@(LatLng (xLat, _)) yLL@(LatLng (yLat, _)) =
    TaskDistance $ earthRadius * radDist 
    where
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
        radDist = 2 * F.atan eps (F.sqrt eps a / F.sqrt eps (1 - a))

distancePointToPoint :: [Zone] -> TaskDistance
distancePointToPoint xs =
    TaskDistance $ sum $ zipWith f ys (tail ys)
    where
        ys = center <$> xs
        unwrap (TaskDistance x) = x
        f = (unwrap .) . distanceHaversine defEps
