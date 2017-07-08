module Flight.Separated (separatedZones) where

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
separated x y =
    d > rxy
    where
        (Radius rx) = radius x
        (Radius ry) = radius y
        rxy = rx + ry
        (TaskDistance d) = distancePointToPoint [x, y]

separatedZones :: [Zone] -> Bool
separatedZones xs =
    and $ zipWith separated xs (tail xs)
