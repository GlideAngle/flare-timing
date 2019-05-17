{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Flight.Track.Cross
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
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
    , InterpolatedFix(..)
    , ZoneTag(..)
    , ZoneCross(..)
    , TrackLogError(..)
    , Fix(..)
    , RetroActive(..)
    , StopWindow(..)
    , trackLogErrors
    , asIfFix
    ) where

import Data.String (IsString())
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Clip (FlyingSection)
import Flight.Pilot (TrackFileFail(..))
import Flight.LatLng.Raw (RawLat, RawLng)
import Flight.Field (FieldOrdering(..))
import Flight.Score (Pilot(..))

-- | For a stopped task, this is the time the task is scored until, the
-- announced stop time wound back by the score back time.
newtype RetroActive = RetroActive UTCTime
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data StopWindow =
    StopWindow
        { lastStarters :: [Pilot]
        -- ^ The pilot or pilots last to start in an elapsed time race or
        -- a race to goal task with multiple start gates. For race to goal
        -- tasks with a single start gate @lastStarters@ will be an empty list.
        , windowTimes :: FlyingSection UTCTime
        -- ^ The scored window as a time range. For an elapsed time race or
        -- a race to goal task with only one start gate this will be the range
        -- from the start until the retroactive stop time.  For race to goal
        -- task with multiple start gates this will be the time available
        -- racing for the last pilot or pilots to start.
        , windowSeconds :: Seconds
        -- ^ The width of the time window.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)


-- | For each task, the crossing for that task.
data Crossing =
    Crossing
        { suspectDnf :: [[Pilot]]
        -- ^ For each task, the pilots whose tracklogs suggest they did not fly
        -- such as by having no fixes.
        , stopWindow :: [Maybe StopWindow]
        -- ^ The scored time window for a stopped task.
        , flying :: [[(Pilot, Maybe TrackFlyingSection)]]
        -- ^ For each task, the pilots' flying sections.
        , crossing :: [[PilotTrackCross]]
        -- ^ For each task, for each made zone, the pair of fixes cross it.
        , trackLogError :: [TrackLogError]
        -- ^ For each task, the pilots with track log problems. Note that
        -- pilots that flew but have no track appear here with
        -- @TrackLogFileNotSet@ as the error and will be awarded minimum
        -- distance.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

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
        , scoredFixes :: FlyingSection Int
        -- ^ A stopped task may cut the flying fixes short.
        , loggedSeconds :: Maybe Seconds
        -- ^ The number of seconds logging fixes.
        , flyingSeconds :: FlyingSection Seconds
        -- ^ The flying section as second offsets from the first fix.
        , scoredSeconds :: FlyingSection Seconds
        -- ^ The scored section as second offsets from the first fix.
        , loggedTimes :: FlyingSection UTCTime
        -- ^ The time range of all fixes logged, not just those flown.
        , flyingTimes :: FlyingSection UTCTime
        -- ^ The flying section as a time range.
        , scoredTimes :: FlyingSection UTCTime
        -- ^ The scored section as a time range.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | For a single track, the zones crossed.
data TrackCross =
    TrackCross
        { zonesCrossSelected :: [Maybe ZoneCross]
        -- ^ The crossing selected as making the zone, for each zone.
        , zonesCrossNominees :: [[Maybe ZoneCross]]
        -- ^ Every crossing of every zone.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | For a single task, which pilots have error detected with their track logs.
data TrackLogError =
    TrackLogError
        { fileUnset :: [Pilot]
        -- ^ Pilots without a track log file.
        , dirMissing :: [Pilot]
        -- ^ A directory part of the log file path is missing.
        , fileMissing :: [Pilot]
        -- ^ The log file is missing.
        , fileUnread :: [Pilot]
        -- ^ The log file could not be read.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

trackLogErrors :: [(Pilot, TrackFileFail)] -> TrackLogError
trackLogErrors xs =
    TrackLogError
        { fileUnset = unsets xs
        , dirMissing = dirs xs
        , fileMissing = files xs
        , fileUnread = unreads xs
        }
    where
        f p = fmap fst . filter (p . snd)
        unsets = f (== TrackLogFileNotSet)
        dirs = f (\case TaskFolderExistsNot _ -> True; _ -> False)
        files = f (\case TrackLogFileExistsNot _ -> True; _ -> False)
        unreads = f (\case TrackLogFileNotRead _ -> True; _ -> False)

-- | A timestamped latitude and longitude.
data Fix =
    Fix { fix :: Int
        -- ^ The 0-based index into the list of fixes from the track log.
        , time :: UTCTime
        -- ^ The time this fix was made.
        , lat :: RawLat
        -- ^ The latitude in decimal degrees, +ve is N and -ve is S.
        , lng :: RawLng
        -- ^ The longitude in decimal degrees, +ve is E and -ve is W.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | An interpolated fix.
data InterpolatedFix =
    InterpolatedFix
        { fixFrac :: Double
        -- ^ The fractional and 0-based index into the list of fixes from the
        -- track log representing how far between the two base fixes the
        -- interpolated point is.
        , time :: UTCTime
        -- ^ The interpolated time.
        , lat :: RawLat
        -- ^ The interpolated latitude in decimal degrees, +ve is N and -ve is S.
        , lng :: RawLng
        -- ^ The interpolated longitude in decimal degrees, +ve is E and -ve is W.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A crossing between two fixes.
data ZoneTag =
    ZoneTag
        { inter :: InterpolatedFix
        , cross :: ZoneCross
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

asIfFix :: ZoneTag -> Fix
asIfFix ZoneTag{inter = InterpolatedFix{fixFrac, time, lat, lng}} =
    Fix
        { fix = round fixFrac
        , time = time
        , lat = lat
        , lng = lng
        }

-- | A pair of fixes that cross a zone.
data ZoneCross =
    ZoneCross
        { crossingPair :: [Fix]
        -- ^ The pair of fixes that cross the zone.
        , inZone :: [Bool]
        -- ^ Mark each fix as inside or outside the zone.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Associates a pilot with the zones they cross for a single task.
data PilotTrackCross =
    PilotTrackCross
        Pilot
        (Maybe TrackCross)
        -- ^ The cross should be Just if the pilot launched.
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance FieldOrdering Crossing where
    fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        ("fileUnset", _) -> LT

        ("dirMissing", "fileUnset") -> GT
        ("dirMissing", _) -> LT

        ("fileMissing", "fileUnset") -> GT
        ("fileMissing", "dirMissing") -> GT
        ("fileMissing", _) -> LT

        ("fileUnread", _) -> GT

        ("suspectDnf", _) -> LT

        ("stopWindow", "suspectDnf") -> GT
        ("stopWindow", _) -> LT

        ("flying", "suspectDnf") -> GT
        ("flying", "stopWindow") -> GT
        ("flying", _) -> LT

        ("crossings", "suspectDnf") -> LT
        ("crossings", "stopWindow") -> LT
        ("crossings", "flying") -> LT
        ("crossings", _) -> LT

        ("trackLogError", _) -> GT

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

        ("scoredFixes", "flyingFixes") -> GT
        ("scoredFixes", "loggedFixes") -> GT
        ("scoredFixes", _) -> LT

        ("loggedSeconds", "loggedFixes") -> GT
        ("loggedSeconds", "flyingFixes") -> GT
        ("loggedSeconds", "scoredFixes") -> GT
        ("loggedSeconds", _) -> LT

        ("flyingSeconds", "loggedFixes") -> GT
        ("flyingSeconds", "flyingFixes") -> GT
        ("flyingSeconds", "scoredFixes") -> GT
        ("flyingSeconds", "loggedSeconds") -> GT
        ("flyingSeconds", _) -> LT

        ("scoredSeconds", "loggedFixes") -> GT
        ("scoredSeconds", "flyingFixes") -> GT
        ("scoredSeconds", "scoredFixes") -> GT
        ("scoredSeconds", "loggedSeconds") -> GT
        ("scoredSeconds", "flyingSeconds") -> GT
        ("scoredSeconds", _) -> LT

        ("loggedTimes", "loggedFixes") -> GT
        ("loggedTimes", "flyingFixes") -> GT
        ("loggedTimes", "loggedSeconds") -> GT
        ("loggedTimes", "flyingSeconds") -> GT
        ("loggedTimes", "scoredSeconds") -> GT
        ("loggedTimes", _) -> LT

        ("flyingTimes", "scoredTimes") -> LT
        ("flyingTimes", _) -> GT
        ("scoredTimes", _) -> GT

        _ -> compare a b
