{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    ( TaskDistance(..)
    , distancePointToPoint
    , distanceHaversineF
    , distanceHaversine
    , fromKms
    ) where

import Prelude hiding (sum)
import Data.Ratio((%))
import qualified Data.Number.FixedFunctions as F
import Data.UnitsOfMeasure (One, (+:), (-:), (*:), u, convert, abs', zero)
import Data.UnitsOfMeasure.Internal (Quantity(..), mk, fromRational')
import Data.Number.RoundingFunctions (dpRound)
import Data.Bifunctor.Flip (Flip(..))

import Flight.Geo
    ( Lat(..)
    , Lng(..)
    , LatLng(..)
    , Epsilon(..)
    , earthRadius
    , defEps
    )
import Flight.Zone (Zone(..), Radius(..), center)

fromKms :: Quantity Rational [u| km |] -> TaskDistance
fromKms q = TaskDistance (convert q)

newtype TaskDistance =
    TaskDistance (Quantity Rational [u| m |])
    deriving (Eq, Ord)

instance Show TaskDistance where
    show (TaskDistance d) = "d = " ++ show dbl
        where
            km = convert d :: Quantity Rational [u| km |]
            Flip rounded = dpRound 6 <$> Flip km
            dbl = fromRational' rounded :: Quantity Double [u| km |]

instance {-# OVERLAPPING #-} Show [TaskDistance] where
    show = showDistances

showDistances :: [TaskDistance] -> String
showDistances xs =
    show (f <$> xs) ++ " km"
    where
        f (TaskDistance d) = show dbl
            where
                km = convert d :: Quantity Rational [u| km |]
                Flip rounded = dpRound 6 <$> Flip km
                (MkQuantity dbl) = fromRational' rounded :: Quantity Double [u| km |]

-- | Sperical distance using haversines and floating point numbers.
distanceHaversineF :: LatLng [u| rad |]
                   -> LatLng [u| rad |]
                   -> TaskDistance
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
distanceHaversine :: Epsilon
                  -> LatLng [u| rad |]
                  -> LatLng [u| rad |]
                  -> TaskDistance
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
distancePointToPoint :: [Zone] -> TaskDistance

distancePointToPoint [] = TaskDistance [u| 0 m |]

distancePointToPoint [_] = TaskDistance [u| 0 m |]

distancePointToPoint [Cylinder (Radius xR) x, Cylinder (Radius yR) y]
    | x == y && xR /= yR = TaskDistance dR
    | otherwise = distancePointToPoint [Point x, Point y]
    where
        dR :: Quantity Rational [u| m |]
        dR = abs' $ xR -: yR

distancePointToPoint xs@[a, b]
    | a == b = TaskDistance zero
    | otherwise = distance xs

distancePointToPoint xs = distance xs

sum :: [Quantity Rational [u| m |]] -> Quantity Rational [u| m |]
sum = foldr (+:) zero

distance :: [Zone] -> TaskDistance
distance xs =
    TaskDistance $ sum $ zipWith f ys (tail ys)
    where
        ys = center <$> xs
        unwrap (TaskDistance x) = x

        f :: LatLng [u| rad |]
          -> LatLng [u| rad |]
          -> Quantity Rational [u| m |]
        f = (unwrap .) . distanceHaversine defEps

