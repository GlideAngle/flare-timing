{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Data.Flight.TrackZone
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Intersection of pilot tracks with competition zones.
-}
module Data.Flight.TrackZone
    ( -- * Track Zone Intersection
      TrackZoneIntersect(..)
    , TaskTrack(..)
    , TrackLine(..)
    , LatLng(..)
    , FlownTrack(..)
    , PilotFlownTrack(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Flight.LatLng (Latitude(..), Longitude(..))
import Data.Flight.Pilot (Pilot(..))

data TrackZoneIntersect =
    TrackZoneIntersect { taskTracks :: [TaskTrack]
                       , pilotTracks :: [[PilotFlownTrack]]
                       } deriving (Show, Generic)

instance ToJSON TrackZoneIntersect
instance FromJSON TrackZoneIntersect

data TaskTrack =
    TaskTrack { pointToPoint :: TrackLine
              , edgeToEdge :: TrackLine
              } deriving (Show, Generic)

instance ToJSON TaskTrack
instance FromJSON TaskTrack

data TrackLine =
    TrackLine { distance :: Double
              , waypoints :: [LatLng]
              } deriving (Show, Generic)

instance ToJSON TrackLine
instance FromJSON TrackLine

data LatLng =
    LatLng { lat :: Latitude
           , lng :: Longitude
           } deriving (Eq, Show, Generic)

instance ToJSON LatLng
instance FromJSON LatLng

data FlownTrack =
    FlownTrack { launched :: Bool
               } deriving (Show, Generic)

instance ToJSON FlownTrack
instance FromJSON FlownTrack

data PilotFlownTrack =
    PilotFlownTrack Pilot (Maybe FlownTrack)
    deriving (Show, Generic)

instance ToJSON PilotFlownTrack
instance FromJSON PilotFlownTrack
