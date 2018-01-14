{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Flight.Gap.Pilots
    ( PilotsAtEss(..)
    , PilotsInGoalAtStop(..)
    , PilotsLaunched(..)
    , PilotsLandedBeforeStop(..)
    ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import GHC.Generics (Generic)

-- | The number of pilots completing the speed section of the task.
newtype PilotsAtEss = PilotsAtEss Integer
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype PilotsInGoalAtStop = PilotsInGoalAtStop Int deriving (Eq, Ord, Show)
newtype PilotsLandedBeforeStop = PilotsLandedBeforeStop Int deriving (Eq, Show)
newtype PilotsLaunched = PilotsLaunched Int deriving (Eq, Show)
