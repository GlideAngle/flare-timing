module Flight.Earth.Sphere (earthRadius) where

import Data.UnitsOfMeasure (u)

import Flight.Units ()
import Flight.Zone(Radius(..), QRadius)

-- | The radius of the earth in the FAI sphere is 6,371 km.
earthRadius :: Num a => QRadius a [u| m |]
earthRadius = Radius [u| 6371000 m |]
