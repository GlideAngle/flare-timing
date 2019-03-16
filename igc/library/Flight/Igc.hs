{-|
Module: Flight.Igc
Copyright:
    © 2017-2019 Phil de Joux
    © 2017-2019 Block Scope Limited
License: MPL-2.0
Maintainer: Phil de Joux <phil.dejoux@blockscope.com>
Stability: experimental

Provides parsing the IGC format for waypoint fixes. The date header is also parsed
as it is needed for the fixes that have only a time and pickup the date in the file
header.
-}
module Flight.Igc
    (
    -- * Data
      IgcRecord(..)
    , HMS(..)
    , Lat(..)
    , Lng(..)
    , AltBaro(..)
    , AltGps(..)
    -- * Parsing
    , parse
    , parseFromFile
    -- * Types
    , Altitude(..)
    , Degree(..)
    , Hour(..)
    , MinuteOfTime(..)
    , MinuteOfAngle(..)
    , Second(..)
    , Year(..)
    , Month(..)
    , Day(..)
    , Nth(..)
    , addHoursIgc
    -- * Record classification
    , isMark
    , isFix
    -- * Fix Checking and Conversion
    , igcEqOrEqOnTime
    , igcBumpOver
    , extract
    , mark
    ) where

import Flight.Igc.Record
import Flight.Igc.Parse
import Flight.Igc.Fix
