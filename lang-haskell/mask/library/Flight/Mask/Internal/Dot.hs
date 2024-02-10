{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Mask.Internal.Dot (dotToGoal) where

import Prelude hiding (span)

import Flight.Kml (MarkedFixes(..))
import Flight.Comp (Task(..), Zones(..))
import Flight.Units ()
import Flight.Distance (TaskDistance(..))
import Flight.Mask.Internal.Zone (TaskZone(..), fixToPoint)
import Flight.Mask.Internal.Race (Reach)
import Flight.Mask.Internal.Cross (crossingPredicates, isStartExit)
import Flight.Geodesy.Solution (SeparatedZones)

-- | The distance to goal checking for each crossing.
dotToGoal
    :: (Real a, Fractional a)
    => SeparatedZones a
    -> (Zones -> [TaskZone a])
    -> Reach _ a _
    -> Task k
    -> MarkedFixes
    -> Maybe (TaskDistance a)
    -- ^ Nothing indicates no such task or a task with no zones.
dotToGoal
    sepZs fromZones dvz
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
                    sepZs
                    (isStartExit sepZs fromZones x)
                    x)
            task
