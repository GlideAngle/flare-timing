{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Earth.Flat.Separated (separatedZones) where

import Prelude hiding (span)
import Data.UnitsOfMeasure ((+:), u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified UTMRef as HC (easting, northing)

import Flight.Units ()
import Flight.Zone (Zone(..), Radius(..), radius)
import Flight.Zone.Path (distancePointToPoint)
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.LatLng (LatLng(..))
import Flight.Earth.Flat.Projected.Internal (zoneToProjectedEastNorth)

boundingBoxSeparated
    :: (Num a, Ord a, Real a, Fractional a)
    => Zone a
    -> Zone a
    -> Bool
boundingBoxSeparated (Point xLL) (Cylinder (Radius ry) yLL) =
    boxSeparated (ry, yLL) xLL
boundingBoxSeparated _ _ = False

boxSeparated
    :: (Num a, Ord a, Real a, Fractional a)
    => (Quantity a [u| m |], LatLng a [u| rad |])
    -> LatLng a [u| rad |]
    -> Bool
boxSeparated (MkQuantity r', yLL) xLL =
    case ( zoneToProjectedEastNorth (Point xLL)
         , zoneToProjectedEastNorth (Point yLL)
         ) of
        (Right x, Right y) ->
            dN > r && dE > r
            where
                dN = yN - xN
                dE = yE - xE

                xN = HC.northing x
                yN = HC.northing y

                xE = HC.easting x
                yE = HC.easting y

        _ -> error "Cannot project lat/lng to UTM easting/northing"
    where
        r = realToFrac r'

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
