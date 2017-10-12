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
    , ZoneProof(..)
    , Fix(..)
    ) where

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Flight.Pilot (Pilot(..))
import Flight.LatLng.Raw (RawLat, RawLng)

newtype PilotTracks =
    PilotTracks { pilotTracks :: [[PilotFlownTrack]] }
    deriving (Show, Generic)

instance ToJSON PilotTracks
instance FromJSON PilotTracks

data FlownTrack =
    FlownTrack { launched :: Bool
               , madeGoal :: Bool
               , zonesMade :: [Bool]
               , zonesProof :: [Maybe ZoneProof]
               , timeToGoal :: Maybe Double
               , distanceToGoal :: Maybe Double
               , bestDistance :: Maybe Double
               }
   deriving (Show, Generic)

instance ToJSON FlownTrack
instance FromJSON FlownTrack

data Fix =
    Fix { time :: UTCTime
        , lat :: RawLat
        , lng :: RawLng
        }
   deriving (Show, Generic)

instance ToJSON Fix
instance FromJSON Fix

data ZoneProof =
    ZoneProof { fixes :: [Fix]
              , inZone :: [Bool]
              }
   deriving (Show, Generic)

instance ToJSON ZoneProof
instance FromJSON ZoneProof

data PilotFlownTrack =
    PilotFlownTrack Pilot (Maybe FlownTrack)
    deriving (Show, Generic)

instance ToJSON PilotFlownTrack
instance FromJSON PilotFlownTrack
