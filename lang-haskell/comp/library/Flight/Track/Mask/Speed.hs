{-|
Module      : Flight.Track.Mask.Speed
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks masked with task control zones.
-}
module Flight.Track.Mask.Speed (MaskingSpeed(..)) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Distance (QTaskDistance)
import Flight.LatLng (QAlt)
import Flight.Score (Pilot(..), BestTime(..))
import Flight.Field (FieldOrdering(..))
import Flight.Units ()
import Flight.Track.Speed (TrackSpeed(..))
import Flight.Track.Mask.Cmp (cmp)

-- | For each task, the masking for speed for that task.
data MaskingSpeed =
    MaskingSpeed
        { ssBestTime :: ![Maybe (BestTime (Quantity Double [u| h |]))]
        -- ^ For each task, the best time ignoring start gates.
        , gsBestTime :: ![Maybe (BestTime (Quantity Double [u| h |]))]
        -- ^ For each task, the best time from the start gate taken.
        , taskDistance :: ![Maybe (QTaskDistance Double [u| m |])]
        -- ^ For each task, the task distance.
        , taskSpeedDistance :: [Maybe (QTaskDistance Double [u| m |])]
        -- ^ For each task, the speed section subset of the task distance.
        , ssSpeed :: ![[(Pilot, TrackSpeed)]]
        -- ^ For each task, for each pilot making goal, their time for the
        -- speed section and speed fraction, ignoring any start gates.
        , gsSpeed :: ![[(Pilot, TrackSpeed)]]
        -- ^ For each task, for each pilot making goal, their time for the
        -- speed section and speed fraction, taking into account start gates.
        , altStopped :: ![[(Pilot, QAlt Double [u| m |])]]
        -- ^ For each task, the altitude of each pilot at the score back time.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance FieldOrdering MaskingSpeed where fieldOrder _ = cmp
