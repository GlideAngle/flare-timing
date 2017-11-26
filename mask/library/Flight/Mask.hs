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
    ( SigMasking
    , TaskZone
    , Ticked(..)
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
    ) where

import Flight.Mask.Tag
import Flight.Mask.Distance
import Flight.Mask.Time
import Flight.Mask.Tracks
import Flight.Mask.Internal (TaskZone, Ticked(..), zoneToCylinder, slice)
