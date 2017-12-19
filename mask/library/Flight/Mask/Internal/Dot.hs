{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Mask.Internal.Dot (dotToGoal) where

import Prelude hiding (span)

import Flight.Kml (MarkedFixes(..))
import qualified Flight.Zone.Raw as Raw (RawZone(..))
import Flight.Comp (Task(..), SpeedSection)
import Flight.Units ()
import Flight.Distance (TaskDistance(..))
import Flight.Task (SpanLatLng)
import Flight.Mask.Internal.Zone (TaskZone(..), TrackZone(..), fixToPoint)
import Flight.Mask.Internal.Race (Sliver(..), Reach)
import Flight.Mask.Internal.Cross (CrossingPredicate, crossingPredicates, isStartExit)

-- | The distance to goal checking for each crossing.
dotToGoal
    :: (Real b, Fractional b)
    => SpanLatLng b
    -> (Raw.RawZone -> TaskZone b)
    -> Reach _ _ _
    -> Task
    -> MarkedFixes
    -> Maybe (TaskDistance b)
    -- ^ Nothing indicates no such task or a task with no zones.
dotToGoal
    span zoneToCyl dvz task@Task{speedSection, zones} MarkedFixes{fixes} =
    if null zones then Nothing else
    dvz
        fixToPoint
        speedSection
        fs
        (zoneToCyl <$> zones)
        fixes 
    where
        fs =
            (\x ->
                crossingPredicates
                    span
                    (isStartExit span zoneToCyl x)
                    x)
            task

stepToGoal
    :: forall a b. (Real b, Fractional b)
    => [CrossingPredicate a b]
    -> Sliver b
    -> (a -> TrackZone b)
    -> SpeedSection
    -> [TaskZone b]
    -> [a]
    -> Maybe (TaskDistance b)
stepToGoal = undefined
