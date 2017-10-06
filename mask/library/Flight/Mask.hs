module Flight.Mask (Predicate) where

import qualified Data.Flight.Kml as Kml (MarkedFixes(..))
import qualified Data.Flight.Comp as Cmp (Task(..))
import Data.Flight.TrackLog as Log (IxTask(..))

type Predicate = [Cmp.Task] -> IxTask -> Kml.MarkedFixes -> Bool
