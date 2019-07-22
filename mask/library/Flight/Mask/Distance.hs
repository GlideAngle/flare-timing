module Flight.Mask.Distance
    ( dashDistancesToGoal
    , dashDistanceToGoal
    , dashPathToGoalMarkedFixes
    , dashPathToGoalTimeRows
    , togoAtLanding
    , madeAtLanding
    ) where

import Data.Time.Clock (UTCTime)
import Data.List (inits)
import Data.UnitsOfMeasure ((-:), u, toRational')

import Flight.Clip (FlyCut(..), FlyClipping(..))
import Flight.Kml (MarkedFixes(..))
import qualified Flight.Kml as Kml (Fix)
import Flight.Track.Time (ZoneIdx(..), TimeRow(..))
import Flight.Track.Cross (Fix(..))
import Flight.Comp (EarthMath(..), Task(..), Zones(..))
import Flight.Units ()
import Flight.Mask.Internal.Zone (TaskZone(..), fixFromFix, fixToPoint, rowToPoint)
import Flight.Mask.Internal.Race (Ticked)
import Flight.Mask.Internal.Dash (dashPathToGoalR, dashToGoalR)
import Flight.Distance (PathDistance(..), QTaskDistance, TaskDistance(..))
import Flight.Task (Zs(..), fromZs)
import Flight.Span.Math (Math(..))
import Flight.Span.Sliver (Sliver(..))
import qualified Flight.Span.Double as Dbl (fromZones, sliver)
import qualified Flight.Span.Rational as Rat (fromZones, sliver, fromR)

dashDistancesToGoal
    :: (Real a, Fractional a, FlyClipping UTCTime MarkedFixes)
    => Ticked
    -> Sliver a
    -> (Zones -> [TaskZone a])
    -> Task k
    -> FlyCut UTCTime MarkedFixes
    -> Maybe [(Maybe Fix, Maybe (QTaskDistance a [u| m |]))]
    -- ^ Nothing indicates no such task or a task with no zones.
dashDistancesToGoal
    ticked sliver fromZones
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
    if null (raw zones) then Nothing else Just
    $ lfg fromZones task mark0
    <$> drop 1 (inits ixs)
    where
        lfg = lastFixToGoal ticked sliver
        ixs = index fixes
        FlyCut{uncut = MarkedFixes{mark0, fixes}} = clipToCut flyCut

dashDistanceToGoal
    :: (Real a, Fractional a, FlyClipping UTCTime MarkedFixes)
    => Ticked
    -> Sliver a
    -> (Zones -> [TaskZone a])
    -> Task k
    -> FlyCut UTCTime MarkedFixes
    -> Maybe (QTaskDistance a [u| m |])
dashDistanceToGoal
    ticked sliver fromZones task flyCut =
    fromZs
    $ edgesSum
    <$> dashPathToGoalMarkedFixes ticked sliver fromZones task flyCut

dashPathToGoalTimeRows
    :: (Real a, Fractional a, FlyClipping UTCTime [TimeRow])
    => Ticked
    -> Sliver a
    -> (Zones -> [TaskZone a])
    -> Task k
    -> FlyCut UTCTime [TimeRow]
    -> Zs (PathDistance a)
    -- ^ Nothing indicates no such task or a task with no zones.
dashPathToGoalTimeRows
    ticked sliver fromZones Task{speedSection, zones} flyCut =

    if null (raw zones) then Z0 else
    dashPathToGoalR ticked sliver rowToPoint speedSection zs ixs
    where
        zs = fromZones zones
        ixs = revindex fixes
        FlyCut{uncut = fixes} = clipToCut flyCut

dashPathToGoalMarkedFixes
    :: (Real a, Fractional a, FlyClipping UTCTime MarkedFixes)
    => Ticked
    -> Sliver a
    -> (Zones -> [TaskZone a])
    -> Task k
    -> FlyCut UTCTime MarkedFixes
    -> Zs (PathDistance a)
    -- ^ Nothing indicates no such task or a task with no zones.
dashPathToGoalMarkedFixes
    ticked sliver fromZones Task{speedSection, zones} flyCut =

    if null (raw zones) then Z0 else
    dashPathToGoalR ticked sliver fixToPoint speedSection zs ixs
    where
        zs = fromZones zones
        ixs = revindex fixes
        FlyCut{uncut = MarkedFixes{fixes}} = clipToCut flyCut

revindex :: [a] -> [(ZoneIdx, a)]
revindex = reverse . index

-- | The distance from the last fix to goal passing through the remaining
-- control zones.
lastFixToGoal
    :: (Real a, Fractional a)
    => Ticked -- ^ The zones ticked
    -> Sliver a
    -> (Zones -> [TaskZone a])
    -> Task k
    -> UTCTime
    -> [(ZoneIdx, Kml.Fix)]
    -> (Maybe Fix, Maybe (QTaskDistance a [u| m |]))
lastFixToGoal
    ticked
    sliver
    fromZones
    Task{speedSection, zones}
    mark0
    ixs =
    case iys of
        [] -> (Nothing, Nothing)
        ((i, y) : _) -> (Just $ fixFromFix mark0 i y, d)
    where
        d = dashToGoalR ticked sliver fixToPoint speedSection zs iys
        zs = fromZones zones
        iys = reverse ixs

dashDistanceFlown
    :: (Real a, Fractional a, FlyClipping UTCTime MarkedFixes)
    => QTaskDistance a [u| m |]
    -> Ticked
    -> Sliver a
    -> (Zones -> [TaskZone a])
    -> Task k
    -> FlyCut UTCTime MarkedFixes
    -> Maybe (QTaskDistance a [u| m |])
dashDistanceFlown
    (TaskDistance dTask)
    ticked
    sliver
    fromZones
    Task{speedSection, zones}
    flyCut =
    if null zs then Nothing else do
        TaskDistance dPilot
            <- dashToGoalR ticked sliver fixToPoint speedSection zs ixs

        return . TaskDistance $ dTask -: dPilot
    where
        zs = fromZones zones
        ixs = reverse . index $ fixes
        FlyCut{uncut = MarkedFixes{fixes}} = clipToCut flyCut

index :: [a] -> [(ZoneIdx, a)]
index = zip $ ZoneIdx <$> [1 .. ]

togoAtLanding
    :: Math
    -> EarthMath
    -> Ticked
    -> Task k
    -> FlyCut UTCTime MarkedFixes
    -> Maybe (QTaskDistance Double [u| m |])
togoAtLanding math earthMath ticked task xs =
    case math of
        Floating ->
            dashDistanceToGoal
                ticked
                (Dbl.sliver earthMath)
                (Dbl.fromZones earthMath)
                task
                xs

        Rational ->
            Rat.fromR <$>
            dashDistanceToGoal
                ticked
                (Rat.sliver earthMath)
                (Rat.fromZones earthMath)
                task
                xs
madeAtLanding
    :: Math
    -> EarthMath
    -> QTaskDistance Double [u| m |]
    -> Ticked
    -> Task k
    -> FlyCut UTCTime MarkedFixes
    -> Maybe (QTaskDistance Double [u| m |]) 
madeAtLanding math earthMath dTaskF@(TaskDistance td) ticked task xs =
    case math of
        Floating ->
            dashDistanceFlown
                dTaskF
                ticked
                (Dbl.sliver earthMath)
                (Dbl.fromZones earthMath)
                task
                xs

        Rational ->
            Rat.fromR <$>
            dashDistanceFlown
                (TaskDistance $ toRational' td)
                ticked
                (Rat.sliver earthMath)
                (Rat.fromZones earthMath)
                task
                xs
