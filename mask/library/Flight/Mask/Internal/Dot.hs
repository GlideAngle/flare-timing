{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Mask.Internal.Dot (dotToGoal) where

import Prelude hiding (span)

import Flight.LatLng (AzimuthFwd)
import Flight.Kml (MarkedFixes(..))
import Flight.Comp (Task(..), Zones(..))
import Flight.Units ()
import Flight.Distance (TaskDistance(..), SpanLatLng)
import Flight.Mask.Internal.Zone (TaskZone(..), fixToPoint)
import Flight.Mask.Internal.Race (Reach)
import Flight.Mask.Internal.Cross (crossingPredicates, isStartExit)

-- | The distance to goal checking for each crossing.
dotToGoal
    :: (Real b, Fractional b)
    => AzimuthFwd a
    -> SpanLatLng b
    -> (Zones -> [TaskZone b])
    -> Reach _ _ _
    -> Task k
    -> MarkedFixes
    -> Maybe (TaskDistance b)
    -- ^ Nothing indicates no such task or a task with no zones.
dotToGoal
    az span fromZones dvz
    task@Task{speedSection, zones}
    MarkedFixes{fixes} =
    if null zs
       then Nothing
       else dvz fixToPoint speedSection fs zs fixes
    where
        zs = fromZones zones

        fs =
            (\x ->
                crossingPredicates
                    az
                    span
                    (isStartExit az span fromZones x)
                    x)
            task
