module WireTypes.Reach
    ( ReachFraction(..)
    , TrackReach(..)
    , BolsterStats(..)
    , dfNoTrackReach
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))

import WireTypes.Point (PilotDistance(..))
import WireTypes.ValidityWorking (ReachStats(..))
import WireTypes.Route (TaskDistance(..))
import WireTypes.Pilot (Pilot, AwardedDistance(..), DfNoTrackPilot(..))

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

data BolsterStats =
    BolsterStats
        { bolster :: ReachStats
        , reach :: ReachStats
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

dfNoTrackReach :: TaskDistance -> DfNoTrackPilot -> (Pilot, TrackReach)
dfNoTrackReach (TaskDistance td) DfNoTrackPilot{pilot, awardedReach} =
    (pilot,) $
    maybe
        (TrackReach (PilotDistance 0) (ReachFraction 0))
        (\AwardedDistance{awardedFrac = af} ->
            TrackReach (PilotDistance $ af * td) (ReachFraction af))
        awardedReach
