{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Data.Flight.PilotTrack
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Intersection of pilot tracks with task control zones.
-}
module Data.Flight.PilotTrack
    ( -- * For each task.
      Crossing(..)
    , Tagging(..)
    , Masking(..)
      -- * For each track.
    , TrackTime(..)
    , TrackCross(..)
    , TrackTag(..)
    , TrackMask(..)
      -- * Associated with a pilot.
    , PilotTrackCross(..)
    , PilotTrackTag(..)
    , PilotTrackMask(..)
      -- * A single fix and a crossing as a pair of fixes.
    , ZoneCross(..)
    , Fix(..)
    ) where

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Flight.Pilot (Pilot(..))
import Flight.LatLng.Raw (RawLat, RawLng)

-- | For each task, the crossing for that task.
newtype Crossing =
    Crossing
        { crossing :: [[PilotTrackCross]]
          -- ^ For each made zone, the pair of fixes cross it.
        }
    deriving (Show, Generic)

instance ToJSON Crossing
instance FromJSON Crossing

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

-- | For each task, the masking for that task.
newtype Masking =
    Masking { masking :: [[PilotTrackMask]] }
    deriving (Show, Generic)

instance ToJSON Masking
instance FromJSON Masking

-- | The timing and tagging for a single task.
data TrackTime =
    TrackTime
        { zonesFirst :: [Maybe UTCTime]
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

data TrackMask =
    TrackMask
        { madeGoal :: Bool
        -- ^ Was goal made.
        , timeToGoal :: Maybe Double
        -- ^ How long did this pilot take to complete the course.
        , distanceToGoal :: Maybe Double
        -- ^ The shortest distance to goal of any fix in the track.
        , distanceMade :: Maybe Double
        -- ^ The task distance minus the distance to goal.
        }
   deriving (Show, Generic)

instance ToJSON TrackMask
instance FromJSON TrackMask

-- | For a single track, the zones crossed.
newtype TrackCross =
    TrackCross
        { zonesCross :: [Maybe ZoneCross]
        -- ^ The cross for each made zone.
        }
   deriving (Show, Generic)

instance ToJSON TrackCross
instance FromJSON TrackCross

-- | For a single track, the interpolated fix for each zone tagged.
newtype TrackTag =
    TrackTag
        { zonesTag :: [Maybe Fix]
        -- ^ The interpolated fix tagging each made zone.
        }
   deriving (Show, Generic)

instance ToJSON TrackTag
instance FromJSON TrackTag

-- | A timestamped latitude and longitude.
data Fix =
    Fix { time :: UTCTime
        , lat :: RawLat
        , lng :: RawLng
        }
   deriving (Show, Generic)

instance ToJSON Fix
instance FromJSON Fix

-- | A pair of fixes that cross a zone.
data ZoneCross =
    ZoneCross
        { crossingPair :: [Fix]
        -- ^ The pair of fixes that cross the zone.
        , inZone :: [Bool]
        -- ^ Mark each fix as inside or outside the zone.
        }
   deriving (Show, Generic)

instance ToJSON ZoneCross
instance FromJSON ZoneCross

-- | Associates a pilot with a flight summary (mask) for a single task.
data PilotTrackMask =
    PilotTrackMask
        Pilot
        (Maybe TrackMask)
        -- ^ The task summary (mask) should be Just if the pilot launched.
    deriving (Show, Generic)

instance ToJSON PilotTrackMask
instance FromJSON PilotTrackMask

-- | Associates a pilot with the zones they cross for a single task.
data PilotTrackCross =
    PilotTrackCross
        Pilot
        (Maybe TrackCross)
        -- ^ The cross should be Just if the pilot launched.
    deriving (Show, Generic)

instance ToJSON PilotTrackCross
instance FromJSON PilotTrackCross

-- | Associates a pilot with the zones they tag for a single task.
data PilotTrackTag =
    PilotTrackTag
        Pilot
        (Maybe TrackTag)
        -- ^ The tags should be Just if the pilot launched.
    deriving (Show, Generic)

instance ToJSON PilotTrackTag
instance FromJSON PilotTrackTag
