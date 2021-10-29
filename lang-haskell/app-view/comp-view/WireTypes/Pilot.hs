module WireTypes.Pilot
    ( PilotTaskStatus(..)
    , Pilot(..)
    , PilotId(..)
    , PilotName(..)
    , Dnf(..)
    , DfNoTrack(..)
    , DfNoTrackPilot(..)
    , Nyp(..)
    , Penal(..)
    , AwardedDistance(..)
    , AwardedVelocity(..)
    , fstPilotId
    , getPilotId
    , getPilotName
    , nullPilot
    , pilotIdsWidth
    ) where

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))
import WireTypes.Point (ReachToggle(..))
import WireTypes.Penalty (PenaltySeqs)

-- | The group of pilots that were penalized for a task.
newtype Penal = Penal {unPenal :: [(Pilot, PenaltySeqs, String)]}

-- | The group of pilots that did not fly a task.
newtype Dnf = Dnf {unDnf :: [Pilot]}

data AwardedDistance = AwardedDistance {awardedFrac :: Double}
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data AwardedVelocity =
    AwardedVelocity
        { ss :: Maybe UTCTime
        , es :: Maybe UTCTime
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data DfNoTrackPilot =
    DfNoTrackPilot
        { pilot :: Pilot
        , awardedReach :: Maybe (ReachToggle AwardedDistance)
        , awardedVelocity :: AwardedVelocity
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

-- | The group of pilots that did fly but do not have a track log.
newtype DfNoTrack =
    DfNoTrack
        {unDfNoTrack :: [DfNoTrackPilot]}
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

-- | The group of pilots not yet processed.
newtype Nyp = Nyp {unNyp :: [Pilot]}

data PilotTaskStatus
    = ABS | DF | DFNoTrack | DNF | NYP
    deriving (Eq, Ord, Show, Generic, FromJSON)

newtype PilotId =
    PilotId String 
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype PilotName =
    PilotName String
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype Pilot = Pilot (PilotId, PilotName)
    deriving (Eq, Show, Generic)
    deriving anyclass (FromJSON)

-- | Order by name then by id.
instance Ord Pilot where
    (Pilot (k0, s0)) `compare` (Pilot (k1, s1)) =
        (s0, k0) `compare` (s1, k1)

fstPilotId :: (Pilot, a) -> (PilotId, a)
fstPilotId (Pilot (pid, _), x) = (pid, x)

getPilotId :: Pilot -> PilotId
getPilotId (Pilot (pid, _)) = pid

getPilotName :: Pilot -> PilotName
getPilotName (Pilot (_, name)) = name

nullPilot :: Pilot
nullPilot = Pilot (PilotId "", PilotName "")

pilotIdWidth :: Pilot -> Int
pilotIdWidth (Pilot (PilotId pid, _)) =
    length pid

pilotIdsWidth :: [Pilot] -> Int
pilotIdsWidth [] = 0
pilotIdsWidth ps =
    maximum $ pilotIdWidth <$> ps
