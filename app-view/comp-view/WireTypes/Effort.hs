module WireTypes.Effort
    ( EffortFraction(..)
    , TrackEffort(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))

import WireTypes.Point (PilotDistance(..))

newtype EffortFraction = EffortFraction Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data TrackEffort =
    TrackEffort
        { effort :: PilotDistance
        , frac :: EffortFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)
