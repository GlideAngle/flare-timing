{-# LANGUAGE RankNTypes #-}

module Flight.Mask.Internal.Dash (dashToGoal, dashToGoalR) where

import Prelude hiding (span)

import Flight.Comp (SpeedSection)
import Flight.Units ()
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Task (distanceEdgeToEdge)
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
    -> [a]
    -> Maybe (TaskDistance b)
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
    -> [a]
    -> Maybe (TaskDistance b)
dashToGoalR _ _ _ _ _ [] =
    Nothing

dashToGoalR
    RaceSections{race = []} Sliver{..} mkZone speedSection zs (x : _) =
    -- NOTE: Didn't make the start so skip the start.
    Just . edgesSum
    $ distanceEdgeToEdge span dpp cseg cs cut mm30 (cons mkZone x zsSkipStart)
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        zsSkipStart = unTaskZone <$> drop 1 zsSpeed

dashToGoalR
    RaceSections{race} Sliver{..} mkZone speedSection zs (x : _) =
    -- NOTE: I don't consider all fixes from last turnpoint made
    -- so this distance is the distance from the very last fix when
    -- at times on this leg the pilot may have been closer to goal.
    Just . edgesSum
    $ distanceEdgeToEdge span dpp cseg cs cut mm30 (cons mkZone x zsNotTicked)

    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        zsNotTicked = unTaskZone <$> drop (length race) zsSpeed

