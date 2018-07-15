module Flight.Zone
    ( HasArea(..)
    , QRadius
    , Radius(..)
    , QIncline
    , Incline(..)
    , QBearing
    , Bearing(..)
    , Zone(..)
    , Deadline(..)
    , TimeOfDay(..)
    , Interval(..)
    , StartGates(..)
    , Task(..)
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

    , RawZoneToZone
    , rawZonesToZones
    ) where

import Flight.Zone.Radius (Radius(..), QRadius)
import Flight.Zone.Bearing (Bearing(..), QBearing)
import Flight.Zone.Incline (Incline(..), QIncline)
import Flight.Zone.Zone
    ( HasArea(..), Zone(..), RawZoneToZone
    , center, radius, showZoneDMS, rawZonesToZones
    )
import Flight.Zone.Convert
    ( fromRationalLatLng, toRationalLatLng
    , fromRationalRadius, toRationalRadius
    , fromRationalZone, toRationalZone
    , realToFracLatLng, realToFracZone, realToFracRadius
    )

newtype Deadline = Deadline Integer deriving (Eq, Ord, Show)
newtype TimeOfDay = TimeOfDay Rational deriving (Eq, Ord, Show)
newtype Interval = Interval Rational deriving (Eq, Ord, Show)

data StartGates
    = StartGates
        { open :: TimeOfDay
        , intervals :: [Interval]
        } deriving Show

data Task a
    = Task
        { zones :: [Zone a]
        , startZone :: Int
        , endZone :: Int
        , startGates :: StartGates
        , deadline :: Maybe Deadline
        }
