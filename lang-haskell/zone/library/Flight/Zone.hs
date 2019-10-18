module Flight.Zone
    ( HasArea(..)
    , QRadius
    , Radius(..)
    , QAltTime
    , AltTime(..)
    , QIncline
    , Incline(..)
    , QBearing
    , ArcSweep(..)
    , Bearing(..)
    , showZoneDMS
    , center
    , radius
    , fromRationalRadius
    , fromRationalZone
    , toRationalRadius
    , toRationalZone
    , realToFracRadius
    , realToFracZone
    , fromRationalLatLng
    , toRationalLatLng
    , realToFracLatLng

    -- * Zones
    , Zone(..)
    , RawZoneToZone
    , rawZonesToZones
    , toCylinder
    , rawToLatLng
    , unlineZones
    ) where

import Flight.Zone.Radius (Radius(..), QRadius)
import Flight.Zone.Bearing (Bearing(..), QBearing, ArcSweep(..))
import Flight.Zone.AltTime (AltTime(..), QAltTime)
import Flight.Zone.Incline (Incline(..), QIncline)
import Flight.Zone.Zone
    ( HasArea(..), Zone(..), RawZoneToZone
    , center, radius, showZoneDMS, rawZonesToZones
    , toCylinder, rawToLatLng, unlineZones
    )
import Flight.Zone.Convert
    ( fromRationalLatLng, toRationalLatLng
    , fromRationalRadius, toRationalRadius
    , fromRationalZone, toRationalZone
    , realToFracLatLng, realToFracZone, realToFracRadius
    )

