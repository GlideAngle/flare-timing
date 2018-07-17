{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Mask.Internal.Dot (dotToGoal) where

import Prelude hiding (span)

import Flight.Kml (MarkedFixes(..))
import qualified Flight.Zone.Raw as Raw (RawZone(..))
import Flight.Comp (Task(..))
import Flight.Units ()
import Flight.Distance (TaskDistance(..), SpanLatLng)
import Flight.Mask.Internal.Zone (TaskZone(..), fixToPoint)
import Flight.Mask.Internal.Race (Reach)
import Flight.Mask.Internal.Cross (crossingPredicates, isStartExit)

-- | The distance to goal checking for each crossing.
dotToGoal
    :: (Real b, Fractional b)
    => SpanLatLng b
    -> (Raw.RawZone -> TaskZone b)
    -> Reach _ _ _
    -> Task k
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
