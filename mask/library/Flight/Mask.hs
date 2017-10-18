module Flight.Mask (SigMasking) where

import Flight.Kml (MarkedFixes)
import Flight.Comp (Task)
import Flight.TrackLog (IxTask)

-- | A masking produces a value from a task and tracklog fixes.
type SigMasking a = [Task] -> IxTask -> MarkedFixes -> a
