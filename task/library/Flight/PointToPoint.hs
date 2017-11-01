{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Flight.PointToPoint
    ( distancePointToPoint
    , distanceHaversineF
    , distanceHaversine
    , costSegment
    , SpanLatLng
    ) where

import Prelude hiding (sum, span)
import Data.Ratio((%))
import qualified Data.Number.FixedFunctions as F
import Data.UnitsOfMeasure (One, (+:), (-:), (*:), u, abs', zero, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..), mk)

import Flight.LatLng
    ( Lat(..)
    , Lng(..)
    , LatLng(..)
    , Epsilon(..)
    , earthRadius
    )
import Flight.Zone (Zone(..), Radius(..), center)
import Flight.Distance (TaskDistance(..), PathDistance(..))

-- | A function for measuring the distance between two points given as
-- latitude longitude pairs in radians.
type SpanLatLng = LatLng [u| rad |] -> LatLng [u| rad |] -> TaskDistance

costSegment :: Real a => SpanLatLng -> Zone a -> Zone a -> PathDistance
costSegment span x y = distancePointToPoint span [x, y]

-- | Sperical distance using haversines and floating point numbers.
distanceHaversineF :: SpanLatLng
distanceHaversineF xDegreeLL yDegreeLL =
    TaskDistance $ radDist *: earthRadius
    where
        -- NOTE: Use xLatF etc to avoid an hlint duplication warning.
        LatLng (Lat xLatF, Lng xLngF) = xDegreeLL
        LatLng (Lat yLatF, Lng yLngF) = yDegreeLL
        (dLatF, dLngF) = (yLatF -: xLatF, yLngF -: xLngF)

        haversine :: Quantity Rational [u| rad |]
                  -> Quantity Double [u| rad |]
        haversine (MkQuantity x) =
            MkQuantity $ y * y
            where
                y :: Double
                y = sin $ fromRational (x * (1 % 2))

        a :: Double
        a =
            hLatF
            + cos (fromRational xLatF')
            * cos (fromRational yLatF')
            * hLngF
            where
                (MkQuantity xLatF') = xLatF
                (MkQuantity yLatF') = yLatF
                (MkQuantity hLatF) = haversine dLatF
                (MkQuantity hLngF) = haversine dLngF

        radDist :: Quantity Rational One
        radDist = mk $ toRational $ 2 * asin (sqrt a)

-- | Sperical distance using haversines and rational numbers.
distanceHaversine :: Epsilon -> SpanLatLng
distanceHaversine (Epsilon eps) xLL yLL =
    TaskDistance $ radDist *: earthRadius
    where
        LatLng (Lat xLat, Lng xLng) = xLL
        LatLng (Lat yLat, Lng yLng) = yLL
        (dLat, dLng) = (yLat -: xLat, yLng -: xLng)

        haversine :: Quantity Rational [u| rad |]
                  -> Quantity Rational [u| rad |]
        haversine (MkQuantity x) =
            MkQuantity $ y * y
            where
                y :: Rational
                y = F.sin eps (x * (1 % 2))

        a :: Rational
        a =
            hLat
            + F.cos eps xLat'
            * F.cos eps yLat'
            * hLng
            where
                (MkQuantity xLat') = xLat
                (MkQuantity yLat') = yLat
                (MkQuantity hLat) = haversine dLat
                (MkQuantity hLng) = haversine dLng

        radDist :: Quantity Rational One
        radDist = mk $ 2 * F.asin eps (F.sqrt eps a)

-- | One way of measuring task distance is going point-to-point through each control
-- zone's center along the course from start to goal. This is not used by CIVL
-- but sometimes task distance will be reported this way.
--
-- The speed section  usually goes from start exit cylinder to goal cylinder
-- or to goal line. The optimal way to fly this in a zig-zagging course will
-- avoid zone centers for a shorter flown distance.
distancePointToPoint :: Real a => SpanLatLng -> [Zone a] -> PathDistance
distancePointToPoint span xs =
    PathDistance
        { edgesSum = distanceViaCenters span xs
        , vertices = center <$> xs
        }

distanceViaCenters :: Real a => SpanLatLng -> [Zone a] -> TaskDistance

distanceViaCenters _ [] = TaskDistance [u| 0 m |]

distanceViaCenters _ [_] = TaskDistance [u| 0 m |]

distanceViaCenters span [Cylinder (Radius xR) x, Cylinder (Radius yR) y]
    | x == y && xR /= yR = TaskDistance dR
    | otherwise = distanceViaCenters span ([Point x, Point y] :: [Zone Rational])
    where
        xR' = toRational' xR
        yR' = toRational' yR

        dR :: Quantity Rational [u| m |]
        dR = abs' $ xR' -: yR'

distanceViaCenters span xs@[a, b]
    | a == b = TaskDistance zero
    | otherwise = distance span xs

distanceViaCenters span xs = distance span xs

sum :: [Quantity Rational [u| m |]] -> Quantity Rational [u| m |]
sum = foldr (+:) zero

distance :: SpanLatLng -> [Zone a] -> TaskDistance
distance span xs =
    TaskDistance $ sum $ zipWith f ys (tail ys)
    where
        ys = center <$> xs
        unwrap (TaskDistance x) = x

        f :: LatLng [u| rad |]
          -> LatLng [u| rad |]
          -> Quantity Rational [u| m |]
        f = (unwrap .) . span
