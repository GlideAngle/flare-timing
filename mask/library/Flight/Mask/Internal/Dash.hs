module Flight.Mask.Internal.Dash
    ( dashToGoal
    , dashPathToGoalR
    , dashToGoalR
    ) where

import Prelude hiding (span)
import Data.UnitsOfMeasure (u)

import Flight.Comp (SpeedSection)
import Flight.Track.Time (ZoneIdx(..))
import Flight.Units ()
import Flight.Distance (QTaskDistance, PathDistance(..))
import Flight.Task (Zs(..), distanceEdgeToEdge, fromZs)
import Flight.Mask.Internal.Zone (TaskZone(..), TrackZone(..), slice)
import Flight.Mask.Internal.Race (Sliver(..), RaceSections(..), Ticked, cons, mm30)

-- | The distance to goal given the zones have been ticked.
dashToGoal
    :: forall a b. (Real b, Fractional b)
    => Ticked
    -> Sliver b
    -> (a -> TrackZone b)
    -> SpeedSection
    -> [TaskZone b]
    -> [(ZoneIdx, a)]
    -> Maybe (QTaskDistance b [u| m |])
dashToGoal ticked sliver mkZone speedSection zs xs =
    dashToGoalR ticked sliver mkZone speedSection zs (reverse xs)

-- | Same as @dashToGoal@ but taking the fixes reversed.
dashToGoalR
    :: forall a b. (Real b, Fractional b)
    => Ticked
    -> Sliver b
    -> (a -> TrackZone b)
    -> SpeedSection
    -> [TaskZone b]
    -> [(ZoneIdx, a)]
    -> Maybe (QTaskDistance b [u| m |])
dashToGoalR ticked sliver mkZone speedSection zs xs =
    fromZs
    $ edgesSum
    <$> dashPathToGoalR ticked sliver mkZone speedSection zs xs

dashPathToGoalR
    :: forall a b. (Real b, Fractional b)
    => Ticked
    -> Sliver b
    -> (a -> TrackZone b)
    -> SpeedSection
    -> [TaskZone b]
    -> [(ZoneIdx, a)]
    -> Zs (PathDistance b)
dashPathToGoalR _ _ _ _ _ [] =
    Z0

dashPathToGoalR
    RaceSections{race = []} Sliver{..} mkZone speedSection zs ((_, x) : _) =
    -- NOTE: Didn't make the start so skip the start.
    distanceEdgeToEdge span dpp cseg cs angleCut mm30 (cons mkZone x zsSkipStart)
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        zsSkipStart = unTaskZone <$> drop 1 zsSpeed

dashPathToGoalR
    RaceSections{race} Sliver{..} mkZone speedSection zs ((_, x) : _) =
    -- NOTE: I don't consider all fixes from last turnpoint made
    -- so this distance is the distance from the very last fix when
    -- at times on this leg the pilot may have been closer to goal.
    distanceEdgeToEdge span dpp cseg cs angleCut mm30 (cons mkZone x zsNotTicked)
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        zsNotTicked = unTaskZone <$> drop (length race) zsSpeed
