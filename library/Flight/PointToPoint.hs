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

newtype TaskDistance = TaskDistance Rational deriving (Eq, Ord, Show)

center :: Zone -> LatLng
center (Point x) = x
center (Vector _ x) = x
center (Cylinder _ x) = x
center (Conical _ _ x) = x
center (Line _ x) = x
center (SemiCircle _ x) = x

distanceHaversineF :: LatLng -> LatLng -> TaskDistance
distanceHaversineF (LatLng (xLat, xLng)) (LatLng (yLat, yLng)) =
    TaskDistance $ earthRadius * toRational radDist 
    where
        distLat :: Rational
        distLat = yLat - xLat
         
        distLng :: Rational
        distLng = yLng - xLng

        haversine :: Rational -> Double
        haversine x =
            y * y
            where
                y :: Double
                y = sin $ fromRational (x * (1 % 2))

        a :: Double
        a =
            haversine distLat
            + cos (fromRational xLat)
            * cos (fromRational yLat)
            * haversine distLng

        radDist :: Double
        radDist = 2 * atan2 (sqrt a) (sqrt $ 1 - a)

distanceHaversine :: Epsilon -> LatLng -> LatLng -> TaskDistance
distanceHaversine (Epsilon eps) (LatLng (xLat, xLng)) (LatLng (yLat, yLng)) =
    TaskDistance $ earthRadius * radDist 
    where
        distLat :: Rational
        distLat = yLat - xLat
         
        distLng :: Rational
        distLng = yLng - xLng

        haversine :: Rational -> Rational
        haversine x =
            y * y
            where
                y :: Rational
                y = F.sin eps (x * (1 % 2))

        a :: Rational
        a =
            haversine distLat
            + F.cos eps xLat
            * F.cos eps yLat
            * haversine distLng

        radDist :: Rational
        radDist = 2 * F.atan eps ((F.sqrt eps a) / (F.sqrt eps $ 1 - a))

distancePointToPoint :: [Zone] -> TaskDistance
distancePointToPoint xs =
    TaskDistance $ sum $ zipWith f ys (tail ys)
    where
        ys = center <$> xs
        unwrap (TaskDistance x) = x
        f = (unwrap .) . distanceHaversine defEps
