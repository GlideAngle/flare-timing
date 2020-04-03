module Internal.ClearlySeparated (clearlySeparatedZones) where

import Prelude hiding (span)
import Data.UnitsOfMeasure ((+:))

import Flight.Units ()
import Flight.Zone (Zone(..), Radius(..), radius)
import Flight.Zone.Path (distancePointToPoint)
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)

clearlySeparatedZones :: Real a => SpanLatLng a -> Zone a -> Zone a -> Bool
clearlySeparatedZones span x y =
    d > rxy
    where
        (Radius rx) = radius x
        (Radius ry) = radius y
        rxy = rx +: ry
        (TaskDistance d) = edgesSum $ distancePointToPoint span [x, y]
