{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Flight.Gap.Pilots
    ( PilotsAtEss(..)
    , PilotsPresent(..)
    , PilotsFlying(..)
    , PilotsInGoalAtStop(..)
    , PilotsLaunched(..)
    , PilotsLandedBeforeStop(..)
    ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import GHC.Generics (Generic)

-- | The number of pilots completing the speed section of the task.
newtype PilotsAtEss = PilotsAtEss Integer
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | The number of pilots present for a task.
newtype PilotsPresent = PilotsPresent Int deriving (Eq, Ord, Show)

-- | The number of pilots flying a task.
newtype PilotsFlying = PilotsFlying Int deriving (Eq, Ord, Show)

-- | The number of pilots that launched on a task.
newtype PilotsLaunched = PilotsLaunched Int deriving (Eq, Show)

-- | The number of pilots having made goal when the task is stopped.
newtype PilotsInGoalAtStop = PilotsInGoalAtStop Int deriving (Eq, Ord, Show)

-- | The number of pilots already having landed out when the task is stopped.
newtype PilotsLandedBeforeStop = PilotsLandedBeforeStop Int deriving (Eq, Show)
