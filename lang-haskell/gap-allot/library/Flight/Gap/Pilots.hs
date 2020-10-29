module Flight.Gap.Pilots
    ( PilotId(..)
    , PilotName(..)
    , Pilot(..)
    , PilotsAtEss(..)
    , PilotsPresent(..)
    , PilotsFlying(..)
    , PilotsLanded(..)
    , PilotsLaunched(..)
    ) where

import Control.DeepSeq
import Data.Aeson (ToJSON(..), FromJSON(..))
import GHC.Generics (Generic)

instance Show PilotId where show (PilotId x) = show x
instance Show PilotName where show (PilotName x) = show x
instance Show Pilot where show (Pilot x) = show x

newtype PilotId =
    PilotId String
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, NFData)

newtype PilotName =
    PilotName String
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, NFData)

newtype Pilot = Pilot (PilotId, PilotName)
    deriving (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON, NFData)

-- | Order by name then by id.
instance Ord Pilot where
    (Pilot (k0, s0)) `compare` (Pilot (k1, s1)) =
        (s0, k0) `compare` (s1, k1)

-- | The number of pilots completing the speed section of the task.
newtype PilotsAtEss = PilotsAtEss Integer
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | The number of pilots present for a task.
{-@ newtype PilotsPresent = PilotsPresent {x :: Integer} @-}
newtype PilotsPresent = PilotsPresent Integer
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | The number of pilots flying a task.
{-@ newtype PilotsFlying = PilotsFlying {x :: Integer} @-}
newtype PilotsFlying = PilotsFlying Integer
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | The number of pilots having landed.
newtype PilotsLanded = PilotsLanded Integer
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | The number of pilots that launched on a task.
newtype PilotsLaunched = PilotsLaunched Int deriving (Eq, Show)
