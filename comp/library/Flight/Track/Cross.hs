{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Data.String (IsString())
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Flight.Pilot (Pilot(..), TrackFileFail)
import Flight.LatLng.Raw (RawLat, RawLng)
import Flight.Field (FieldOrdering(..))
import Flight.Comp (FlyingSection)
import Data.Aeson.Via.Scientific (ViaSci(..))

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
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- NOTE: There's a similar Seconds newtype in the flight-kml package.  I don't
-- want a dependency between these packages so I'm duplicating the newtype
-- here.
newtype Seconds = Seconds Integer
    deriving (Eq, Ord, Show, Generic)
    deriving newtype Num
    deriving anyclass (ToJSON, FromJSON)

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
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | For a single track, the zones crossed.
data TrackCross =
    TrackCross
        { zonesCrossSelected :: [Maybe ZoneCross]
        -- ^ The crossing selected as making the zone, for each zone.
        , zonesCrossNominees :: [[Maybe ZoneCross]]
        -- ^ Every crossing of every zone.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | A timestamped latitude and longitude.
data Fix =
    Fix { fix :: Int
        -- ^ The 0-based index into the list of fixes from the track log.
        , time :: UTCTime
        -- ^ The time this fix was made.
        , lat :: ViaSci RawLat
        -- ^ The latitude in decimal degrees, +ve is N and -ve is S.
        , lng :: ViaSci RawLng
        -- ^ The longitude in decimal degrees, +ve is E and -ve is W.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | A pair of fixes that cross a zone.
data ZoneCross =
    ZoneCross
        { crossingPair :: [Fix]
        -- ^ The pair of fixes that cross the zone.
        , inZone :: [Bool]
        -- ^ Mark each fix as inside or outside the zone.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | Associates a pilot with the zones they cross for a single task.
data PilotTrackCross =
    PilotTrackCross
        Pilot
        (Maybe TrackCross)
        -- ^ The cross should be Just if the pilot launched.
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance FieldOrdering Crossing where
    fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        ("errors", _) -> LT
        ("crossings", "errors") -> GT
        ("crossings", _) -> LT
        ("flying", _) -> GT

        ("zonesCrossSelected", _) -> LT
        ("zonesCrossNominees", _) -> GT

        ("fix", _) -> LT
        ("time", "fix") -> GT
        ("time", _) -> LT
        ("lat", "fix") -> GT
        ("lat", "time") -> GT
        ("lat", _) -> LT
        ("lng", _) -> GT

        ("loggedFixes", _) -> LT
        ("flyingFixes", "loggedFixes") -> GT
        ("flyingFixes", _) -> LT
        ("loggedSeconds", "loggedFixes") -> GT
        ("loggedSeconds", "flyingFixes") -> GT
        ("loggedSeconds", _) -> LT
        ("flyingSeconds", "loggedFixes") -> GT
        ("flyingSeconds", "flyingFixes") -> GT
        ("flyingSeconds", "loggedSeconds") -> GT
        ("flyingSeconds", _) -> LT
        ("loggedTimes", "loggedFixes") -> GT
        ("loggedTimes", "flyingFixes") -> GT
        ("loggedTimes", "loggedSeconds") -> GT
        ("loggedTimes", "flyingSeconds") -> GT
        ("loggedTimes", _) -> LT
        ("flyingTimes", _) -> GT

        _ -> compare a b
