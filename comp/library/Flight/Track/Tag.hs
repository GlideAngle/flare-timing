{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Flight.Track.Tag
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks tagging task control zones.
-}
module Flight.Track.Tag
    ( Tagging(..)
    , TrackTime(..)
    , TrackTag(..)
    , PilotTrackTag(..)
    ) where

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Flight.Pilot (Pilot(..))
import Flight.Track.Cross (Fix)

-- | For each task, the timing and tagging for that task.
data Tagging =
    Tagging
        { timing :: [TrackTime]
          -- ^ For each made zone, the first and last tag.
        , tagging :: [[PilotTrackTag]]
          -- ^ For each made zone, the tag.
        }
    deriving (Show, Generic)

instance ToJSON Tagging
instance FromJSON Tagging

-- | The timing and tagging for a single task.
data TrackTime =
    TrackTime
        { zonesSum :: [Int]
        -- ^ For each zone, the number of pilots tagging the zone.
        , zonesFirst :: [Maybe UTCTime]
        -- ^ For each zone, the time of the first tag.
        , zonesLast :: [Maybe UTCTime]
        -- ^ For each zone, the time of the last tag.
        , zonesRankTime :: [[UTCTime]]
        -- ^ For each zone, the ordered times of each tag.
        , zonesRankPilot :: [[Pilot]]
        -- ^ For each zone, the ordered pilots of each tag.
        }
    deriving (Show, Generic)

instance ToJSON TrackTime
instance FromJSON TrackTime

-- | For a single track, the interpolated fix for each zone tagged.
newtype TrackTag =
    TrackTag
        { zonesTag :: [Maybe Fix]
        -- ^ The interpolated fix tagging each made zone.
        }
   deriving (Show, Generic)

instance ToJSON TrackTag
instance FromJSON TrackTag

-- | Associates a pilot with the zones they tag for a single task.
data PilotTrackTag =
    PilotTrackTag
        Pilot
        (Maybe TrackTag)
        -- ^ The tags should be Just if the pilot launched.
    deriving (Show, Generic)

instance ToJSON PilotTrackTag
instance FromJSON PilotTrackTag
