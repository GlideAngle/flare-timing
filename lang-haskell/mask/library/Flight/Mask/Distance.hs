module Flight.Mask.Distance
    ( GeoDash(..)
    , revindex
    , index
    ) where

import Data.Time.Clock (UTCTime)
import Data.UnitsOfMeasure (u)

import Flight.Units ()
import Flight.Clip (FlyCut(..), FlyClipping(..))
import Flight.Zone.Cylinder (SampleParams(..), ZonePoint(..))
import Flight.Zone.Raw (Give)
import Flight.Kml (MarkedFixes(..))
import qualified Flight.Kml as Kml (Fix)
import Flight.Track.Time (ZoneIdx(..), TimeRow(..))
import Flight.Track.Cross (Fix(..))
import Flight.Comp (Task(..))
import Flight.Task (Zs(..))
import Flight.Distance (PathDistance(..), QTaskDistance)

import Flight.Mask.Internal.Race (Ticked)
import Flight.Geodesy.Solution (Trig, GeodesySolutions(..))

import Flight.Span.Sliver (GeoSliver(..))

revindex :: [a] -> [(ZoneIdx, a)]
revindex = reverse . index

index :: [a] -> [(ZoneIdx, a)]
index = zip $ ZoneIdx <$> [1 .. ]

class GeoSliver g a => GeoDash g a where
    dashDistancesToGoal
        :: (Trig g a, FlyClipping UTCTime MarkedFixes)
        => Earth g
        -> Maybe Give
        -> SampleParams g
        -> Ticked
        -> Task k
        -> FlyCut UTCTime MarkedFixes
        -> Maybe [(Maybe Fix, Maybe (QTaskDistance g [u| m |]))]
        -- ^ Nothing indicates no such task or a task with no zones.

    dashDistanceToGoal
        :: (Trig g a, FlyClipping UTCTime MarkedFixes)
        => Earth g
        -> Maybe Give
        -> SampleParams g
        -> Ticked
        -> Task k
        -> FlyCut UTCTime MarkedFixes
        -> Maybe (QTaskDistance g [u| m |])

    dashPathToGoalTimeRows
        :: (Trig g a, FlyClipping UTCTime [TimeRow])
        => Earth g
        -> Maybe Give
        -> SampleParams g
        -> Ticked
        -> Task k
        -> FlyCut UTCTime [TimeRow]
        -> Zs (PathDistance g)
        -- ^ Nothing indicates no such task or a task with no zones.

    dashPathToGoalMarkedFixes
        :: (Trig g a, FlyClipping UTCTime MarkedFixes)
        => Earth g
        -> Maybe Give
        -> SampleParams g
        -> Ticked
        -> Task k
        -> FlyCut UTCTime MarkedFixes
        -> Zs (PathDistance g)
        -- ^ Nothing indicates no such task or a task with no zones.

    -- | The distance from the last fix to goal passing through the remaining
    -- control zones.
    lastFixToGoal
        :: Trig g a
        => Earth g
        -> Maybe Give
        -> SampleParams g
        -> Maybe [ZonePoint g]
        -> Ticked -- ^ The zones ticked
        -> Task k
        -> UTCTime
        -> [(ZoneIdx, Kml.Fix)]
        -> (Maybe Fix, Maybe (QTaskDistance g [u| m |]))

    dashDistanceFlown
        :: (Trig g a, FlyClipping UTCTime MarkedFixes)
        => Earth g
        -> Maybe Give
        -> SampleParams g
        -> QTaskDistance g [u| m |]
        -> Ticked
        -> Task k
        -> FlyCut UTCTime MarkedFixes
        -> Maybe (QTaskDistance g [u| m |])

    togoAtLanding
        :: Trig g a
        => Earth g
        -> Maybe Give
        -> SampleParams g
        -> Ticked
        -> Task k
        -> FlyCut UTCTime MarkedFixes
        -> Maybe (QTaskDistance g [u| m |])

    madeAtLanding
        :: Trig g a
        => Earth g
        -> Maybe Give
        -> SampleParams g
        -> QTaskDistance g [u| m |]
        -> Ticked
        -> Task k
        -> FlyCut UTCTime MarkedFixes
        -> Maybe (QTaskDistance g [u| m |])
