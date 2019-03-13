module Flight.Mask.Internal.Dash
    ( dashToGoal
    , dashPathToGoalR
    , dashToGoalR
    ) where

import Prelude hiding (span)
import Data.UnitsOfMeasure (u)

import Flight.Zone.SpeedSection (SpeedSection)
import Flight.Track.Time (ZoneIdx(..))
import Flight.Units ()
import Flight.Distance (QTaskDistance, PathDistance(..))
import Flight.Task (Zs(..), distanceEdgeToEdge, fromZs)
import Flight.Mask.Internal.Zone (TaskZone(..), TrackZone(..), slice)
import Flight.Mask.Internal.Race (RaceSections(..), Ticked, mm30)
import Flight.Earth.Sphere.Separated (separatedZones)
import Flight.Zone (Zone(..))
import Flight.Span.Sliver (Sliver(..))

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
    => Ticked -- ^ The zones ticked
    -> Sliver b
    -> (a -> TrackZone b)
    -> SpeedSection
    -> [TaskZone b] -- ^ Zones of the task
    -> [(ZoneIdx, a)] -- ^ Index and distance of each fix
    -> Zs (PathDistance b)
dashPathToGoalR _ _ _ _ _ [] =
    Z0

dashPathToGoalR
    RaceSections{race = []}
    sliver@Sliver{..}
    mkZone
    speedSection
    zs
    ((_, x) : _) =
    -- NOTE: Didn't make the start so skip the start.
    distanceEdgeToEdge span dpp cseg cs angleCut mm30 zs'
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        zsSkipStart = unTaskZone <$> drop 1 zsSpeed
        zs' = zsToCheck sliver mkZone x zsSkipStart

dashPathToGoalR
    RaceSections{race} -- ^ The zones ticked
    sliver@Sliver{..}
    mkZone
    speedSection
    zs
    ((_, x) : _) =
    -- NOTE: I don't consider all fixes from last turnpoint made
    -- so this distance is the distance from the very last fix when
    -- at times on this leg the pilot may have been closer to goal.
    distanceEdgeToEdge span dpp cseg cs angleCut mm30 zs'
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        zsNotTicked = unTaskZone <$> drop (length race) zsSpeed
        zs' = zsToCheck sliver mkZone x zsNotTicked

-- | If the fix I'm checking is inside the first zone then I want to
-- ignore that first zone.
zsToCheck
    :: (Fractional b, Real b)
    => Sliver b
    -> (a -> TrackZone b)
    -> a
    -> [Zone b]
    -> [Zone b]
zsToCheck sliver@Sliver{..} mkZone x zsNotTicked =
    let z = unTrackZone $ mkZone x in
    case zsNotTicked of
        [] -> z : zsNotTicked
        (z0 : zs') ->
            if separatedZones span [z, z0]
                then z : zsNotTicked
                else zsToCheck sliver mkZone x zs'
