module Flight.Separated (separatedZones) where

import Data.Ratio ((%))
import Flight.Zone (Zone(..), Radius(..), radius)
import Flight.PointToPoint (TaskDistance(..), distancePointToPoint)

separated :: Zone -> Zone -> Bool

separated x@(Point _) y@(Point _) =
    x /= y

separated x@(Point _) y =
    d > ry
    where
        (Radius ry) = radius y
        (TaskDistance d) = distancePointToPoint [x, y]

separated x y@(Point _) =
    d > rx
    where
        (Radius rx) = radius x
        (TaskDistance d) = distancePointToPoint [x, y]

-- | Consider cylinders separated if one fits inside the other or if they don't
-- touch.
separated xc@(Cylinder (Radius xR) x) yc@(Cylinder (Radius yR) y)
    | x == y && xR /= yR = True
    | dxy < dR = True
    | otherwise = clearlySeparated xc yc
    where
        (TaskDistance dxy) = distancePointToPoint [Point x, Point y]
        dR = abs $ xR - yR

separated x y =
    clearlySeparated x y

separatedZones :: [Zone] -> Bool
separatedZones xs =
    and $ zipWith separated xs (tail xs)

clearlySeparated :: Zone -> Zone -> Bool
clearlySeparated x y =
    d > rxy
    where
        (Radius rx) = radius x
        (Radius ry) = radius y
        rxy = rx + ry
        (TaskDistance d) = distancePointToPoint [x, y]
