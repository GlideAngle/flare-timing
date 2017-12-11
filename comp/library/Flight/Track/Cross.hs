{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
    , Seconds(..)
    , TrackFlyingSection(..)
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
import Flight.Comp (FlyingSection)

-- | For each task, the crossing for that task.
data Crossing =
    Crossing
        { errors :: [[(Pilot, TrackFileFail)]]
          -- ^ For each task, the pilots with track log problems.
        , flying :: [[(Pilot, Maybe TrackFlyingSection)]]
          -- ^ For each task, the pilots' flying sections.
        , crossing :: [[PilotTrackCross]]
          -- ^ For each task, for each made zone, the pair of fixes cross it.
        }
    deriving (Show, Generic)

instance ToJSON Crossing
instance FromJSON Crossing

-- NOTE: There's a similar Seconds newtype in the flight-kml package.  I don't
-- want a dependency between these packages so I'm duplicating the newtype
-- here.
newtype Seconds = Seconds Integer deriving (Show, Eq, Ord, Num, Generic)

instance ToJSON Seconds
instance FromJSON Seconds

-- | For a single track, the flying section.
data TrackFlyingSection =
    TrackFlyingSection
        { loggedFixes :: Maybe Int
        -- ^ The number of logged fixes.
        , flyingFixes :: FlyingSection Int
        -- ^ The flying section as indices into the list of fixes.
        , loggedSeconds :: Maybe Seconds
        -- ^ The number of seconds logging fixes.
        , flyingSeconds :: FlyingSection Seconds
        -- ^ The flying section as second offsets from the first fix.
        , loggedTimes :: FlyingSection UTCTime
        -- ^ The time range of all fixes logged, not just those flown.
        , flyingTimes :: FlyingSection UTCTime
        -- ^ The flying section as a time range.
        }
   deriving (Show, Generic)

instance ToJSON TrackFlyingSection
instance FromJSON TrackFlyingSection

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
