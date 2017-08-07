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
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

data TrackZoneIntersect =
    TrackZoneIntersect { taskTracks :: [TaskTrack]
                       } deriving (Show, Generic)

instance ToJSON TrackZoneIntersect
instance FromJSON TrackZoneIntersect

data TaskTrack =
    TaskTrack { distance :: Double
              } deriving (Show, Generic)

instance ToJSON TaskTrack
instance FromJSON TaskTrack
