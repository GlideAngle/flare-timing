module Flight.Earth.ZoneShape (onLine) where

import Data.Maybe (catMaybes)
import Data.UnitsOfMeasure ((-:), u, abs')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units.Angle (halfPi)
import Flight.Zone (Bearing(..))
import Flight.Zone.Cylinder (ZonePoint(..))

onLine
    :: (Fractional a, Real a)
    => Maybe (Quantity a [u| rad |])
    -> ([ZonePoint a], [b]) -> ([ZonePoint a], [b])
onLine Nothing xs = xs
onLine (Just theta) (xs, cs) =
    unzip . catMaybes $
    [ if (abs' (b -: theta)) < halfPi then Nothing else Just (x, c)
    | x@ZonePoint{radial = Bearing b} <- xs
    | c <- cs
    ]
