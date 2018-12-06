{-# LANGUAGE DuplicateRecordFields #-}

module WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , LaunchValidityWorking(..)
    , PilotsPresent()
    , PilotsFlying()
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

data ValidityWorking =
    ValidityWorking 
        { launch :: LaunchValidityWorking
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data LaunchValidityWorking =
    LaunchValidityWorking 
        { flying :: PilotsFlying
        , present :: PilotsPresent
        , nominalLaunch :: NominalLaunch
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype PilotsPresent = PilotsPresent Integer
    deriving (Eq, Ord)
    deriving newtype (Show, ToJSON, FromJSON)

newtype PilotsFlying = PilotsFlying Integer
    deriving (Eq, Ord)
    deriving newtype (Show, ToJSON, FromJSON)

newtype NominalLaunch = NominalLaunch Double
    deriving (Eq, Ord)
    deriving newtype (Show, ToJSON, FromJSON)
