{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Separated (separatedZones) where

import Prelude hiding (span)
import Data.UnitsOfMeasure ((+:), (-:), (*:), u, unQuantity, recip')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Zone (Zone(..), Radius(..), radius)
import Flight.PointToPoint.Segment (SpanLatLng)
import Flight.PointToPoint.Double (distancePointToPoint)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.LatLng (Lat(..), Lng(..), LatLng(..), earthRadius)

boundingBoxSeparated :: (Num a, Ord a, Fractional a)
                     => Zone a
                     -> Zone a
                     -> Bool
boundingBoxSeparated (Point xLL) (Cylinder (Radius ry) yLL) =
    boxSeparated (ry, yLL) xLL
boundingBoxSeparated _ _ = False

boxSeparated :: (Num a, Ord a, Fractional a)
             => (Quantity a [u| m |], LatLng a [u| rad |])
             -> LatLng a [u| rad |]
             -> Bool
boxSeparated
    (r', LatLng (Lat yLat, Lng yLng))
    (LatLng (xLLx, xLLy)) =
        xLo || xHi || yLo || yHi
    where
        -- NOTE: Use *: recip' instead of /: to avoid needing a
        -- Floating constraint that is not available for Rational.
        r :: Quantity _ [u| rad |]
        r = (r' *: recip' earthRadius) *: [u| 1 rad |]

        xLo :: Bool
        xLo = xLat' < MkQuantity (negate 1) 

        xHi :: Bool
        xHi = xLat' > MkQuantity 1

        yLo :: Bool
        yLo = xLng' < MkQuantity (negate 1) 

        yHi :: Bool
        yHi = xLng' > MkQuantity 1

        xZero :: Quantity _ [u| rad |]
        xZero = yLat -: r

        yZero :: Quantity _ [u| rad |]
        yZero = yLng -: r

        xScale :: Quantity _ [u| rad |]
        xScale = (yLat +: r) -: (yLat -: r)

        yScale :: Quantity _ [u| rad |]
        yScale = (yLng +: r) -: (yLng -: r)

        xTranslate :: Lat _ [u| rad |] -> Lat _ [u| rad |]
        xTranslate (Lat lat) =
            Lat ((lat -: xZero) *: scale)
            where
                scale :: Quantity _ [u| 1 1 |]
                scale = MkQuantity (unQuantity xScale)

        yTranslate :: Lng _ [u| rad |] -> Lng _ [u| rad |]
        yTranslate (Lng lng) =
            Lng ((lng -: yZero) *: scale)
            where
                scale :: Quantity _ [u| 1 1 |]
                scale = MkQuantity (unQuantity yScale)

        (Lat xLat') = xTranslate xLLx
        (Lng xLng') = yTranslate xLLy

separated :: (Real a, Fractional a)
          => SpanLatLng a
          -> Zone a
          -> Zone a
          -> Bool

separated _ x@(Point _) y@(Point _) =
    x /= y

separated span x y@(Point _) =
    separated span y x

separated
    span
    x@(Point _)
    y@(Cylinder r _) =
    boundingBoxSeparated x y || d > ry
    where
        (Radius ry) = r
        (TaskDistance d) = edgesSum $ distancePointToPoint span [x, y]

separated span x@(Point _) y =
    d > ry
    where
        (Radius ry) = radius y
        (TaskDistance d) = edgesSum $ distancePointToPoint span [x, y]

-- | Consider cylinders separated if one fits inside the other or if they don't
-- touch.
separated span xc@(Cylinder (Radius xR) x) yc@(Cylinder (Radius yR) y)
    | x == y = xR /= yR
    | dxy + minR < maxR = True
    | otherwise = clearlySeparated span xc yc
    where
        (TaskDistance (MkQuantity dxy)) =
            edgesSum
            $ distancePointToPoint
                span
                ([Point x, Point y] :: [Zone _])

        (MkQuantity minR) = max xR yR
        (MkQuantity maxR) = min xR yR

separated span x y =
    clearlySeparated span x y

-- | Are the control zones separated? This a prerequisite to being able to work
-- out the distance between zones. The one exception is coincident cylinders
-- with different radii. Here the difference in radii is considered to be the
-- distance between them. This will be seen where the smaller concentric cylinder
-- marks the launch and the larger one, as an exit cylinder, marks the start of the
-- speed section.
separatedZones :: (Real a, Fractional a) => SpanLatLng a -> [Zone a] -> Bool
separatedZones span xs =
    and $ zipWith (separated span) xs (tail xs)

clearlySeparated :: Real a => SpanLatLng a -> Zone a -> Zone a -> Bool
clearlySeparated span x y =
    d > rxy
    where
        (Radius rx) = radius x
        (Radius ry) = radius y
        rxy = rx +: ry
        (TaskDistance d) = edgesSum $ distancePointToPoint span [x, y]
