module Flight.Gap.Pilots
    ( PilotId(..)
    , PilotName(..)
    , Pilot(..)
    , PilotsAtEss(..)
    , PilotsPresent(..)
    , PilotsFlying(..)
    , PilotsInGoalAtStop(..)
    , PilotsLaunched(..)
    , PilotsLandedBeforeStop(..)
    ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import GHC.Generics (Generic)

newtype PilotId =
    PilotId String 
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype PilotName =
    PilotName String
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype Pilot = Pilot (PilotId, PilotName)
    deriving (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Order by name then by id.
instance Ord Pilot where
    (Pilot (k0, s0)) `compare` (Pilot (k1, s1)) =
        (s0, k0) `compare` (s1, k1)

-- | The number of pilots completing the speed section of the task.
newtype PilotsAtEss = PilotsAtEss Integer
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | The number of pilots present for a task.
{-@ newtype PilotsPresent = PilotsPresent {x :: Integer } @-}
newtype PilotsPresent = PilotsPresent Integer
    deriving (Eq, Ord, Show)
    deriving newtype (ToJSON, FromJSON)

-- | The number of pilots flying a task.
{-@ newtype PilotsFlying = PilotsFlying {x :: Integer } @-}
newtype PilotsFlying = PilotsFlying Integer
    deriving (Eq, Ord, Show)
    deriving newtype (ToJSON, FromJSON)

-- | The number of pilots that launched on a task.
newtype PilotsLaunched = PilotsLaunched Int deriving (Eq, Show)

-- | The number of pilots having made goal when the task is stopped.
newtype PilotsInGoalAtStop = PilotsInGoalAtStop Int
    deriving (Eq, Ord, Show)

-- | The number of pilots already having landed out when the task is stopped.
newtype PilotsLandedBeforeStop = PilotsLandedBeforeStop Int
    deriving (Eq, Show)
