{-|
Module      : Flight.Mask
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Mask tracks with zones, working out; did the pilot launch, did they make goaland how
long did that take? If they didn't make goal then what zones did they make and what
was the distance to goal?
-}
module Flight.Mask
    ( FnTask
    , FnIxTask
    , TaskZone
    , Ticked
    , RaceSections(..)
    , SelectedCrossings(..)
    , NomineeCrossings(..)
    , MadeZones(..)
    , Sliver(..)
    , countFixes
    , checkTracks
    , madeZones
    , tagZones
    , launched
    , madeGoal
    , started
    , groupByLeg
    , distanceToGoal
    , distancesToGoal
    , distanceFlown
    , timeFlown
    , zoneToCylinder
    , slice
    , section
    , nullFlying
    ) where

import Flight.Mask.Tag
import Flight.Mask.Distance
import Flight.Mask.Time
import Flight.Mask.Tracks
import Flight.Mask.Internal.Zone (TaskZone, zoneToCylinder, slice)
import Flight.Mask.Internal.Distance (Sliver(..), Ticked, RaceSections(..), section)
