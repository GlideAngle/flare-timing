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
    , PilotCrossings(..)
    , FlownTrack(..)
    , FlownTrackCrossing(..)
    , PilotFlownTrack(..)
    , PilotFlownTrackCrossing(..)
    , Fix(..)
    , ZoneCrossing(..)
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

newtype PilotCrossings =
    PilotCrossings
        { pilotCrossings :: [[PilotFlownTrackCrossing]]
          -- ^ For each made zone, the pair of fixes on either side of the
          -- crossing.
        }
    deriving (Show, Generic)

instance ToJSON PilotCrossings
instance FromJSON PilotCrossings

data FlownTrack =
    FlownTrack
        { launched :: Bool
        -- ^ Did the pilot launch, inferred from a track with 2+ distinct fixes.
        , madeGoal :: Bool
        -- ^ Was goal made.
        , zonesTime :: [Maybe UTCTime]
        -- ^ For each made zone, when was the crossing made.
        , timeToGoal :: Maybe Double
        -- ^ How long did this pilot take to complete the course.
        , distanceToGoal :: Maybe Double
        -- ^ The shortest distance to goal of any fix in the track.
        , distanceMade :: Maybe Double
        -- ^ The task distance minus the distance to goal.
        }
   deriving (Show, Generic)

instance ToJSON FlownTrack
instance FromJSON FlownTrack

data FlownTrackCrossing =
    FlownTrackCrossing
        { zonesCrossing :: [Maybe ZoneCrossing]
        -- ^ The crossing for each made zone.
        }
   deriving (Show, Generic)

instance ToJSON FlownTrackCrossing
instance FromJSON FlownTrackCrossing

data Fix =
    Fix { time :: UTCTime
        , lat :: RawLat
        , lng :: RawLng
        }
   deriving (Show, Generic)

instance ToJSON Fix
instance FromJSON Fix

data ZoneCrossing =
    ZoneCrossing
        { crossing :: [Fix]
        -- ^ The fixes that cross the zone.
        , inZone :: [Bool]
        -- ^ Marking each fix as inside or outside the zone.
        }
   deriving (Show, Generic)

instance ToJSON ZoneCrossing
instance FromJSON ZoneCrossing

data PilotFlownTrack =
    PilotFlownTrack Pilot (Maybe FlownTrack)
    deriving (Show, Generic)

instance ToJSON PilotFlownTrack
instance FromJSON PilotFlownTrack

data PilotFlownTrackCrossing =
    PilotFlownTrackCrossing Pilot (Maybe FlownTrackCrossing)
    deriving (Show, Generic)

instance ToJSON PilotFlownTrackCrossing
instance FromJSON PilotFlownTrackCrossing
