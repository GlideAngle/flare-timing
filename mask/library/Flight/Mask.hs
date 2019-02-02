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
    ( FnTask
    , FnIxTask
    , TaskZone
    , Ticked
    , RaceSections(..)
    , SelectedCrossings(..)
    , NomineeCrossings(..)
    , MadeZones(..)
    , Sliver(..)
    , FlyClipSection(..)
    , GroupLeg(..)
    , countFixes
    , checkTracks
    , madeZones
    , tagZones
    , launched
    , madeGoal
    , started
    , groupByLeg
    , dashDistancesToGoal
    , dashDistanceToGoal
    , dashPathToGoalMarkedFixes
    , dashPathToGoalTimeRows
    , timeFlown
    , zoneToCylinder
    , slice
    , section
    , nullFlying
    , togoAtLanding
    , madeAtLanding
    , separatedRawZones
    , fixFromFix
    ) where

import Flight.Mask.Group
import Flight.Mask.Tag
import Flight.Mask.Distance
import Flight.Mask.Time
import Flight.Mask.Tracks
import Flight.Mask.Internal.Zone
    (TaskZone, zoneToCylinder, slice, separatedRawZones, fixFromFix)
import Flight.Mask.Internal.Race
    ( FlyClipSection(..)
    , Sliver(..)
    , Ticked
    , RaceSections(..)
    , section
    )
