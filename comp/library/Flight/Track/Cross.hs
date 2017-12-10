{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Flight.Track.Cross
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks crossing task control zones.
-}
module Flight.Track.Cross
    ( Crossing(..)
    , TrackCross(..)
    , PilotTrackCross(..)
    , ZoneCross(..)
    , Fix(..)
    ) where

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Flight.Pilot (Pilot(..), TrackFileFail)
import Flight.LatLng.Raw (RawLat, RawLng)

-- | For each task, the crossing for that task.
data Crossing =
    Crossing
        { crossing :: [[PilotTrackCross]]
          -- ^ For each task, for each made zone, the pair of fixes cross it.
        , errors :: [[(Pilot, TrackFileFail)]]
          -- ^ For each task, the pilots with track log problems.
        }
    deriving (Show, Generic)

instance ToJSON Crossing
instance FromJSON Crossing

-- | For a single track, the zones crossed.
data TrackCross =
    TrackCross
        { zonesCrossSelected :: [Maybe ZoneCross]
        -- ^ The crossing selected as making the zone, for each zone.
        , zonesCrossNominees :: [[Maybe ZoneCross]]
        -- ^ Every crossing of every zone.
        }
   deriving (Show, Generic)

instance ToJSON TrackCross
instance FromJSON TrackCross

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

-- | Associates a pilot with the zones they cross for a single task.
data PilotTrackCross =
    PilotTrackCross
        Pilot
        (Maybe TrackCross)
        -- ^ The cross should be Just if the pilot launched.
    deriving (Show, Generic)

instance ToJSON PilotTrackCross
instance FromJSON PilotTrackCross
