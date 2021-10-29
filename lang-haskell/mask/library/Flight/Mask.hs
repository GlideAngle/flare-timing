-- | NOTE: I get these warnings when reexporting modules that only export
-- typeclass instances.
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

{-|
Module      : Flight.Mask
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Mask tracks with zones, working out; did the pilot launch, did they make goaland how
long did that take? If they didn't make goal then what zones did they make and what
was the distance to goal?
-}
module Flight.Mask
    ( GeoDash(..)
    , GeoLeg(..)
    , GeoTag(..)
    , GeoTime(..)
    , GeoSliver(..)
    , module Dbl
    , module Rat
    , FnTask
    , FnIxTask
    , ZoneTimePass
    , TaskZone
    , Ticked
    , RaceSections(..)
    , SelectedCrossings(..)
    , NomineeCrossings(..)
    , SelectedStart(..)
    , NomineeStarts(..)
    , ExcludedCrossings(..)
    , MadeZones(..)
    , Sliver(..)
    , FlyClipSection(..)
    , GroupLeg(..)
    , checkTracks
    , settingsLogs
    , zonesToTaskZones
    , slice
    , section
    , fixFromFix
    , nullFlying
    ) where

import Flight.Mask.Group
import Flight.Mask.Group.Double as Dbl

import Flight.Mask.Tag
import Flight.Mask.Tag.Double as Dbl
import Flight.Mask.Tag.Motion (nullFlying)

import Flight.Mask.Distance
import Flight.Mask.Distance.Double as Dbl

import Flight.Mask.Time
import Flight.Mask.Time.Double as Dbl

import Flight.Span.Sliver (Sliver(..), GeoSliver(..))
import Flight.Span.Double as Dbl
import Flight.Span.Rational as Rat

import Flight.Mask.Tracks
import Flight.Mask.Internal.Zone (TaskZone, zonesToTaskZones, slice, fixFromFix)
import Flight.Mask.Internal.Race
    ( FlyClipSection(..)
    , Ticked
    , RaceSections(..)
    , section
    )
