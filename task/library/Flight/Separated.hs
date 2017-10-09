{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Flight.Separated (separatedZones) where
    
import Data.UnitsOfMeasure ((+:), (-:), (*:), (/:), u, unQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Zone (Zone(..), Radius(..), radius)
import Flight.PointToPoint (TaskDistance(..), distancePointToPoint)
import Flight.LatLng (Lat(..), Lng(..), LatLng(..), earthRadius)

boundingBoxSeparated :: Quantity Rational [u| m |]
                     -> LatLng [u| rad |]
                     -> LatLng [u| rad |]
                     -> Bool
boundingBoxSeparated
    r'
    (LatLng (xLLx, xLLy))
    (LatLng (Lat yLat, Lng yLng)) =
        xLo || xHi || yLo || yHi
    where
        r :: Quantity Rational [u| rad |]
        r = (r' /: earthRadius) *: [u| 1 rad |]

        xLo :: Bool
        xLo = xLat' < MkQuantity 0

        xHi :: Bool
        xHi = xLat' > MkQuantity 1

        yLo :: Bool
        yLo = xLng' < MkQuantity 0

        yHi :: Bool
        yHi = xLng' > MkQuantity 1

        xZero :: Quantity Rational [u| rad |]
        xZero = yLat -: r

        yZero :: Quantity Rational [u| rad |]
        yZero = yLng -: r

        xScale :: Quantity Rational [u| rad |]
        xScale = (yLat +: r) -: (yLat -: r)

        yScale :: Quantity Rational [u| rad |]
        yScale = (yLng +: r) -: (yLng -: r)

        xTranslate :: Lat [u| rad |] -> Lat [u| rad |]
        xTranslate (Lat lat) =
            Lat ((lat -: xZero) *: scale)
            where
                scale :: Quantity Rational [u| 1 1 |]
                scale = MkQuantity (unQuantity xScale)

        yTranslate :: Lng [u| rad |] -> Lng [u| rad |]
        yTranslate (Lng lng) =
            Lng ((lng -: yZero) *: scale)
            where
                scale :: Quantity Rational [u| 1 1 |]
                scale = MkQuantity (unQuantity yScale)

        (Lat xLat') = xTranslate xLLx
        (Lng xLng') = yTranslate xLLy

separated :: Zone -> Zone -> Bool

separated x@(Point _) y@(Point _) =
    x /= y

separated x y@(Point _) =
    separated y x

separated
    x@(Point xLL)
    y@(Cylinder r yLL) =
    boundingBoxSeparated ry xLL yLL || d > ry
    where
        (Radius ry) = r
        (TaskDistance d) = distancePointToPoint [x, y]

separated x@(Point _) y =
    d > ry
    where
        (Radius ry) = radius y
        (TaskDistance d) = distancePointToPoint [x, y]

-- | Consider cylinders separated if one fits inside the other or if they don't
-- touch.
separated xc@(Cylinder (Radius xR) x) yc@(Cylinder (Radius yR) y)
    | x == y = xR /= yR
    | dxy + minR < maxR = True
    | otherwise = clearlySeparated xc yc
    where
        (TaskDistance (MkQuantity dxy)) =
            distancePointToPoint [Point x, Point y]

        (MkQuantity minR) = max xR yR
        (MkQuantity maxR) = min xR yR

separated x y =
    clearlySeparated x y

-- | Are the control zones separated? This a prerequisite to being able to work
-- out the distance between zones. The one exception is coincident cylinders
-- with different radii. Here the difference in radii is considered to be the
-- distance between them. This will be seen where the smaller concentric cylinder
-- marks the launch and the larger one, as an exit cylinder, marks the start of the
-- speed section.
separatedZones :: [Zone] -> Bool
separatedZones xs =
    and $ zipWith separated xs (tail xs)

clearlySeparated :: Zone -> Zone -> Bool
clearlySeparated x y =
    d > rxy
    where
        (Radius rx) = radius x
        (Radius ry) = radius y
        rxy = rx +: ry
        (TaskDistance d) = distancePointToPoint [x, y]
