{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Data.Flight.PilotTrack
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Intersection of pilot tracks with competition zones.
-}
module Data.Flight.PilotTrack
    ( -- * Pilot Track, Task Control Zone Intersection
      PilotTracks(..)
    , FlownTrack(..)
    , PilotFlownTrack(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Flight.Pilot (Pilot(..))

newtype PilotTracks =
    PilotTracks { pilotTracks :: [[PilotFlownTrack]] } deriving (Show, Generic)

instance ToJSON PilotTracks
instance FromJSON PilotTracks

data FlownTrack =
    FlownTrack { launched :: Bool
               , madeGoal :: Bool
               , zonesMade :: [Bool]
               , timeToGoal :: Maybe Double
               , distanceToGoal :: Maybe Double
               , bestDistance :: Maybe Double
               } deriving (Show, Generic)

instance ToJSON FlownTrack
instance FromJSON FlownTrack

data PilotFlownTrack =
    PilotFlownTrack Pilot (Maybe FlownTrack)
    deriving (Show, Generic)

instance ToJSON PilotFlownTrack
instance FromJSON PilotFlownTrack
