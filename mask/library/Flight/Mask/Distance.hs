module Flight.Mask.Distance
    ( dashDistancesToGoal
    , dashDistanceToGoal
    , dashPathToGoalMarkedFixes
    , dashPathToGoalTimeRows
    , dashDistanceFlown
    ) where

import Data.Time.Clock (UTCTime)
import Data.List (inits)
import Data.UnitsOfMeasure ((-:))
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Kml (MarkedFixes(..))
import qualified Flight.Kml as Kml (Fix)
import Flight.Track.Time (TimeRow(..))
import Flight.Track.Cross (Fix(..))
import Flight.Comp (Task(..))
import Flight.Score (PilotDistance(..))
import Flight.Units ()
import Flight.Mask.Internal.Zone
    (ZoneIdx, TaskZone(..), fixFromFix, fixToPoint, rowToPoint)
import Flight.Mask.Internal.Race (FlyClipping(..), Sliver(..), FlyCut(..), Ticked)
import Flight.Mask.Internal.Dash (dashPathToGoalR, dashToGoalR)
import qualified Flight.Zone.Raw as Raw (RawZone(..))
import Flight.Distance (PathDistance(..), TaskDistance(..))
import Flight.Task (Zs(..), fromZs)

dashDistancesToGoal
    :: (Real a, Fractional a, FlyClipping UTCTime MarkedFixes)
    => Ticked
    -> Sliver a
    -> (Raw.RawZone -> TaskZone a)
    -> Task
    -> FlyCut UTCTime MarkedFixes
    -> Maybe [(Maybe Fix, Maybe (TaskDistance a))]
    -- ^ Nothing indicates no such task or a task with no zones.
dashDistancesToGoal
    ticked sliver zoneToCyl
    task@Task{zones}
    flyCut =
    -- NOTE: A ghci session using inits & tails.
    -- inits [1 .. 4]
    -- [[],[1],[1,2],[1,2,3],[1,2,3,4]]
    --
    -- tails [1 .. 4]
    -- [[1,2,3,4],[2,3,4],[3,4],[4],[]]
    --
    -- tails $ reverse [1 .. 4]
    -- [[4,3,2,1],[3,2,1],[2,1],[1],[]]
    --
    -- drop 1 $ inits [1 .. 4]
    -- [[1],[1,2],[1,2,3],[1,2,3,4]]
    if null zones then Nothing else Just
    $ lfg zoneToCyl task mark0
    <$> drop 1 (inits ixs)
    where
        lfg = lastFixToGoal ticked sliver
        ixs = index fixes
        FlyCut{uncut = MarkedFixes{mark0, fixes}} = clipToFlown flyCut

dashDistanceToGoal
    :: (Real a, Fractional a, FlyClipping UTCTime MarkedFixes)
    => Ticked
    -> Sliver a
    -> (Raw.RawZone -> TaskZone a)
    -> Task
    -> FlyCut UTCTime MarkedFixes
    -> Maybe (TaskDistance a)
dashDistanceToGoal
    ticked sliver zoneToCyl task flyCut =
    fromZs
    $ edgesSum
    <$> dashPathToGoalMarkedFixes ticked sliver zoneToCyl task flyCut
    
dashPathToGoalTimeRows
    :: (Real a, Fractional a, FlyClipping UTCTime [TimeRow])
    => Ticked
    -> Sliver a
    -> (Raw.RawZone -> TaskZone a)
    -> Task
    -> FlyCut UTCTime [TimeRow]
    -> Zs (PathDistance a)
    -- ^ Nothing indicates no such task or a task with no zones.
dashPathToGoalTimeRows
    ticked sliver zoneToCyl Task{speedSection, zones} flyCut =

    if null zones then Z0 else
    dashPathToGoalR ticked sliver rowToPoint speedSection zs ixs
    where
        zs = zoneToCyl <$> zones
        ixs = revindex fixes
        FlyCut{uncut = fixes} = clipToFlown flyCut

dashPathToGoalMarkedFixes
    :: (Real a, Fractional a, FlyClipping UTCTime MarkedFixes)
    => Ticked
    -> Sliver a
    -> (Raw.RawZone -> TaskZone a)
    -> Task
    -> FlyCut UTCTime MarkedFixes
    -> Zs (PathDistance a)
    -- ^ Nothing indicates no such task or a task with no zones.
dashPathToGoalMarkedFixes
    ticked sliver zoneToCyl Task{speedSection, zones} flyCut =

    if null zones then Z0 else
    dashPathToGoalR ticked sliver fixToPoint speedSection zs ixs
    where
        zs = zoneToCyl <$> zones
        ixs = revindex fixes
        FlyCut{uncut = MarkedFixes{fixes}} = clipToFlown flyCut

revindex :: [a] -> [(ZoneIdx, a)]
revindex = reverse . index

-- | The distance from the last fix to goal passing through the remaining
-- control zones.
lastFixToGoal :: (Real a, Fractional a)
              => Ticked
              -> Sliver a
              -> (Raw.RawZone -> TaskZone a)
              -> Task
              -> UTCTime
              -> [(ZoneIdx, Kml.Fix)]
              -> (Maybe Fix, Maybe (TaskDistance a))
lastFixToGoal
    ticked
    sliver
    zoneToCyl
    Task{speedSection, zones}
    mark0
    ixs =
    case iys of
        [] -> (Nothing, Nothing)
        ((i, y) : _) -> (Just $ fixFromFix mark0 i y, d)
    where
        d = dashToGoalR ticked sliver fixToPoint speedSection zs iys
        zs = zoneToCyl <$> zones
        iys = reverse ixs

dashDistanceFlown
    :: (Real a, Fractional a, FlyClipping UTCTime MarkedFixes)
    => TaskDistance a
    -> Ticked
    -> Sliver a
    -> (Raw.RawZone -> TaskZone a)
    -> Task
    -> FlyCut UTCTime MarkedFixes
    -> Maybe (PilotDistance a)
dashDistanceFlown
    (TaskDistance dTask)
    ticked
    sliver
    zoneToCyl
    Task{speedSection, zones}
    flyCut =
    if null zones then Nothing else do
        TaskDistance dPilot
            <- dashToGoalR ticked sliver fixToPoint speedSection zs ixs

        let (MkQuantity diff) = dTask -: dPilot

        return $ PilotDistance diff
    where
        zs = zoneToCyl <$> zones
        ixs = reverse . index $ fixes
        FlyCut{uncut = MarkedFixes{fixes}} = clipToFlown flyCut

index :: [a] -> [(ZoneIdx, a)]
index = zip [1 .. ]
