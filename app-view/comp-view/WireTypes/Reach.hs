module WireTypes.Reach
    ( ReachFraction(..)
    , TrackReach(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))

import WireTypes.Point (PilotDistance(..))

newtype ReachFraction = ReachFraction Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data TrackReach =
    TrackReach
        { reach :: PilotDistance
        , frac :: ReachFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)
