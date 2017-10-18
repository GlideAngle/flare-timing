module Flight.Mask (SigMasking) where

import Data.Flight.Kml (MarkedFixes)
import Flight.Comp (Task)
import Data.Flight.TrackLog (IxTask)

-- | A masking produces a value from a task and tracklog fixes.
type SigMasking a = [Task] -> IxTask -> MarkedFixes -> a
