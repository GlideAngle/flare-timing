module WireTypes.Reach
    ( ReachFraction(..)
    , TrackReach(..)
    , ReachStats(..)
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

data ReachStats =
    ReachStats
        { bolsterMean :: PilotDistance
        , bolsterStdDev :: PilotDistance
        , reachMean :: PilotDistance
        , reachStdDev :: PilotDistance
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)
