module Flight.Mask (Masking) where

import Data.Flight.Kml (MarkedFixes)
import Data.Flight.Comp (Task)
import Data.Flight.TrackLog (IxTask)

-- | A masking produces a value from a task and tracklog fixes.
type Masking a = [Task] -> IxTask -> MarkedFixes -> a
