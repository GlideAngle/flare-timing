{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Earth.Ellipsoid.Separated (separatedZones) where

import Prelude hiding (span)
import Data.UnitsOfMeasure ((+:), (-:), (*:), u, unQuantity, recip')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Zone (Zone(..), Radius(..), radius)
import Flight.Zone.Path (distancePointToPoint)
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Earth.Ellipsoid (Ellipsoid(..), polarRadius)

boundingBoxSeparated
    :: (Num a, Ord a, Fractional a)
    => Ellipsoid a
    -> Zone a
    -> Zone a
    -> Bool
boundingBoxSeparated ellipsoid (Point xLL) (Cylinder (Radius ry) yLL) =
    boxSeparated  ellipsoid (ry, yLL) xLL
boundingBoxSeparated _ _ _ = False

boxSeparated
    :: (Num a, Ord a, Fractional a)
    => Ellipsoid a
    -> (Quantity a [u| m |], LatLng a [u| rad |])
    -> LatLng a [u| rad |]
    -> Bool
boxSeparated
    ellipsoid@Ellipsoid{equatorialR}
    (r', LatLng (Lat yLat, Lng yLng))
    (LatLng (xLLx, xLLy)) =
        xLo || xHi || yLo || yHi
    where
        polarR = polarRadius ellipsoid

        rLat :: Quantity _ [u| rad |]
        rLat = (r' *: recip' polarR) *: [u| 1 rad |]

        rLng :: Quantity _ [u| rad |]
        rLng = (r' *: recip' equatorialR) *: [u| 1 rad |]

        xLo :: Bool
        xLo = xLat' < MkQuantity (negate 1) 

        xHi :: Bool
        xHi = xLat' > MkQuantity 1

        yLo :: Bool
        yLo = xLng' < MkQuantity (negate 1) 

        yHi :: Bool
        yHi = xLng' > MkQuantity 1

        xZero :: Quantity _ [u| rad |]
        xZero = yLat -: rLat

        yZero :: Quantity _ [u| rad |]
        yZero = yLng -: rLng

        xScale :: Quantity _ [u| rad |]
        xScale = (yLat +: rLat) -: (yLat -: rLat)

        yScale :: Quantity _ [u| rad |]
        yScale = (yLng +: rLng) -: (yLng -: rLng)

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

separated
    :: (Real a, Fractional a)
    => Ellipsoid a
    -> SpanLatLng a
    -> Zone a
    -> Zone a
    -> Bool

separated _ _ x@(Point _) y@(Point _) =
    x /= y

separated ellipsoid span x y@(Point _) =
    separated ellipsoid span y x

separated
    ellipsoid
    span
    x@(Point _)
    y@(Cylinder r _) =
    boundingBoxSeparated ellipsoid x y || d > ry
    where
        (Radius ry) = r
        (TaskDistance d) = edgesSum $ distancePointToPoint span [x, y]

separated _ span x@(Point _) y =
    d > ry
    where
        (Radius ry) = radius y
        (TaskDistance d) = edgesSum $ distancePointToPoint span [x, y]

-- | Consider cylinders separated if one fits inside the other or if they don't
-- touch.
separated _ span xc@(Cylinder (Radius xR) x) yc@(Cylinder (Radius yR) y)
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

separated _ span x y =
    clearlySeparated span x y

-- | Are the control zones separated? This a prerequisite to being able to work
-- out the distance between zones. The one exception is coincident cylinders
-- with different radii. Here the difference in radii is considered to be the
-- distance between them. This will be seen where the smaller concentric cylinder
-- marks the launch and the larger one, as an exit cylinder, marks the start of the
-- speed section.
separatedZones
    :: (Real a, Fractional a)
    => Ellipsoid a
    -> SpanLatLng a
    -> [Zone a]
    -> Bool
separatedZones ellipsoid span xs =
    and $ zipWith (separated ellipsoid span) xs (tail xs)

clearlySeparated :: Real a => SpanLatLng a -> Zone a -> Zone a -> Bool
clearlySeparated span x y =
    d > rxy
    where
        (Radius rx) = radius x
        (Radius ry) = radius y
        rxy = rx +: ry
        (TaskDistance d) = edgesSum $ distancePointToPoint span [x, y]
