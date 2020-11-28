{-|
Module      : Flight.Track.Mask.Effort
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks masked with task control zones.
-}
module Flight.Track.Mask.Effort (MaskingEffort(..)) where

import Data.UnitsOfMeasure (u)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Distance (QTaskDistance)
import "flight-gap-allot" Flight.Score (Pilot(..))
import Flight.Field (FieldOrdering(..))
import Flight.Units ()
import Flight.Track.Distance (TrackDistance(..), Effort)
import Flight.Track.Mask.Cmp (cmp)

-- | For each task, the masking for effort for that task.
data MaskingEffort =
    MaskingEffort
        { bestEffort :: [Maybe (QTaskDistance Double [u| m |])]
        -- ^ For each task, the best distance made.
        , land :: [[(Pilot, TrackDistance Effort)]]
        -- ^ For each task, the distance of the landing spot for each pilot
        -- landing out.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance FieldOrdering MaskingEffort where fieldOrder _ = cmp
