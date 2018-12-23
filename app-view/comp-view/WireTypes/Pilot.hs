module WireTypes.Pilot
    ( PilotTaskStatus(..)
    , Pilot(..)
    , PilotId(..)
    , PilotName(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))

data PilotTaskStatus
    = ABS | DF | DNF | NYP
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
