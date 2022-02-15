module Flight.Mask.Internal.Dash
    ( dashToGoal
    , dashPathToGoalR
    , dashToGoalR
    ) where

import Prelude hiding (span)
import Data.UnitsOfMeasure (u)

import Flight.Units ()
import Flight.Zone.SpeedSection (SpeedSection)
import Flight.Zone (Zone(..))
import Flight.Track.Time (ZoneIdx(..))
import Flight.Distance (QTaskDistance, PathDistance(..))
import Flight.Task (Zs(..), fromZs)
import Flight.Geodesy.Solution (SeparatedZones)
import Flight.ShortestPath (OptimalPath)

import Flight.Mask.Internal.Zone (TaskZone(..), TrackZone(..), slice, boundingZone)
import Flight.Mask.Internal.Race (RaceSections(..), Ticked)

-- | The distance to goal given the zones have been ticked.
dashToGoal
    :: forall a b. (Real b, Fractional b)
    => OptimalPath b
    -> SeparatedZones b
    -> Ticked
    -> (a -> TrackZone b)
    -> SpeedSection
    -> [TaskZone b]
    -> [(ZoneIdx, a)]
    -> Maybe (QTaskDistance b [u| m |])
dashToGoal optZs sepZs ticked mkZone speedSection zs xs =
    dashToGoalR optZs sepZs ticked mkZone speedSection zs (reverse xs)

-- | Same as @dashToGoal@ but taking the fixes reversed.
dashToGoalR
    :: forall a b. (Real b, Fractional b)
    => OptimalPath b
    -> SeparatedZones b
    -> Ticked
    -> (a -> TrackZone b)
    -> SpeedSection
    -> [TaskZone b]
    -> [(ZoneIdx, a)]
    -> Maybe (QTaskDistance b [u| m |])
dashToGoalR optZs sepZs ticked mkZone speedSection zs xs =
    fromZs
    $ edgesSum
    <$> dashPathToGoalR optZs sepZs ticked mkZone speedSection zs xs

dashPathToGoalR
    :: forall a b. (Real b, Fractional b)
    => OptimalPath b
    -> SeparatedZones b
    -> Ticked -- ^ The zones ticked
    -> (a -> TrackZone b)
    -> SpeedSection
    -> [TaskZone b] -- ^ Zones of the task
    -> [(ZoneIdx, a)] -- ^ Index and distance of each fix
    -> Zs (PathDistance b)
dashPathToGoalR _ _ _ _ _ _ [] =
    Z0

dashPathToGoalR
    optZs
    sepZs
    RaceSections{race = []}
    mkZone
    speedSection
    zs
    ((_, x) : _) =
    -- NOTE: Didn't make the start so skip the start.
    optZs zs'
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        zsSkipStart = boundingZone <$> drop 1 zsSpeed
        zs' = zsToCheck sepZs mkZone x zsSkipStart

dashPathToGoalR
    optZs
    sepZs
    RaceSections{race} -- ^ The zones ticked
    mkZone
    speedSection
    zs
    ((_, x) : _) =
    -- NOTE: I don't consider all fixes from last turnpoint made
    -- so this distance is the distance from the very last fix when
    -- at times on this leg the pilot may have been closer to goal.
    optZs zs'
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        zsNotTicked = boundingZone <$> drop (length race) zsSpeed
        zs' = zsToCheck sepZs mkZone x zsNotTicked

-- | If the fix I'm checking is inside the first zone then I want to
-- ignore that first zone.
zsToCheck
    :: (Fractional b, Real b)
    => SeparatedZones b
    -> (a -> TrackZone b)
    -> a
    -> [Zone b]
    -> [Zone b]
zsToCheck sepZs mkZone x zsNotTicked =
    let z = unTrackZone $ mkZone x in
    case zsNotTicked of
        [] -> z : zsNotTicked
        (z0 : zs') ->
            if sepZs [z, z0]
                then z : zsNotTicked
                else zsToCheck sepZs mkZone x zs'
