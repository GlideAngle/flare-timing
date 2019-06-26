module WireTypes.Effort (TrackEffort(..)) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))

import WireTypes.Point (PilotDistance(..))
import WireTypes.Fraction (EffortFraction(..))

data TrackEffort =
    TrackEffort
        { effort :: PilotDistance
        , frac :: EffortFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)
