module Flight.Separated (separatedZones) where

import Flight.Zone (Zone(..), Radius(..))
import Flight.PointToPoint (TaskDistance(..), distancePointToPoint)

radius :: Zone -> Radius
radius (Point _) = Radius 0
radius (Vector _ _) = Radius 0
radius (Cylinder x _) = x
radius (Conical _ x _) = x
radius (Line x _) = x
radius (SemiCircle x _) = x

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
