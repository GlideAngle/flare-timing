module WireTypes.Reach
    ( TrackReach(..)
    , BolsterStats(..)
    , dfNoTrackReach
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))

import WireTypes.Point (PilotDistance(..), ReachToggle(..))
import WireTypes.ValidityWorking (ReachStats(..))
import WireTypes.Route (TaskDistance(..))
import WireTypes.Fraction (ReachFraction(..))
import WireTypes.Pilot (Pilot, AwardedDistance(..), DfNoTrackPilot(..))

data TrackReach =
    TrackReach
        { reach :: PilotDistance
        , frac :: ReachFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data BolsterStats =
    BolsterStats
        { bolster :: Maybe ReachStats
        , reach :: Maybe ReachStats
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

dfNoTrackReach :: TaskDistance -> DfNoTrackPilot -> (Pilot, ReachToggle TrackReach)
dfNoTrackReach (TaskDistance td) DfNoTrackPilot{pilot, awardedReach} =
    (pilot,) $
    maybe
        (let r = TrackReach (PilotDistance 0) (ReachFraction 0) in ReachToggle r r)
        (\ReachToggle
            { flown = AwardedDistance{awardedFrac = aF}
            , extra = AwardedDistance{awardedFrac = aE}
            } ->
            ReachToggle
                { flown =
                    TrackReach
                        (PilotDistance $ aF * td)
                        (ReachFraction aF)
                , extra =
                    TrackReach
                        (PilotDistance $ aE * td)
                        (ReachFraction aE)
                })
        awardedReach
