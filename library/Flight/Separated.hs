module Flight.Separated (separatedZones) where

import Flight.Zone (Zone(..), Radius(..))
import Flight.PointToPoint (TaskDistance(..), distancePointToPoint)

radius :: Zone -> Radius
radius (Point _) = Radius 0
radius (Vector _ _) = Radius 0
radius (Cylinder r _) = r
radius (Conical _ r _) = r
radius (Line r _) = r
radius (SemiCircle r _) = r

separated :: Zone -> Zone -> Bool
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
