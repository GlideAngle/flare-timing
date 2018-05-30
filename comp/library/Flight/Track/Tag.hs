{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Flight.Track.Tag
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks tagging task control zones.
-}
module Flight.Track.Tag
    ( Tagging(..)
    , TrackTime(..)
    , TrackTag(..)
    , PilotTrackTag(..)
    , firstLead
    , firstStart
    , lastArrival
    ) where

import Data.String (IsString())
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Flight.Comp (SpeedSection, FirstLead(..), FirstStart(..), LastArrival(..))
import Flight.Pilot (Pilot(..))
import Flight.Track.Cross (Fix)
import Flight.Field (FieldOrdering(..))

-- | For each task, the timing and tagging for that task.
data Tagging =
    Tagging
        { timing :: [TrackTime]
          -- ^ For each made zone, the first and last tag.
        , tagging :: [[PilotTrackTag]]
          -- ^ For each made zone, the tag.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

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
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

firstLead :: SpeedSection -> [Maybe UTCTime] -> Maybe FirstLead
firstLead _ [] = Nothing
firstLead Nothing (t : _) = FirstLead <$> t
firstLead (Just (leg, _)) ts =
    FirstLead <$>
    case drop (leg - 1) ts of
        [] -> Nothing
        (t : _) -> t

firstStart :: SpeedSection -> UTCTime -> [Maybe UTCTime] -> Maybe FirstStart
firstStart _ _ [] = Nothing
firstStart speedSection startTime times =
    -- > or $ Just True
    -- True
    -- > or $ Just False
    -- False
    -- > or $ Nothing
    -- False
    f speedSection $ filter (or . fmap (>= startTime)) times
    where
        f :: SpeedSection -> [Maybe UTCTime] -> Maybe FirstStart
        f _ [] = Nothing
        f Nothing (t : _) = FirstStart <$> t
        f (Just (firstRaceLeg, _)) ts =
            FirstStart <$>
            case drop (firstRaceLeg - 1) ts of
                [] -> Nothing
                (t : _) -> t

lastArrival :: SpeedSection -> [Maybe UTCTime] -> Maybe LastArrival
lastArrival _ [] = Nothing
lastArrival Nothing (t : _) = LastArrival <$> t
lastArrival (Just (_, lastRaceLeg)) ts =
    LastArrival <$>
    case reverse . drop (lastRaceLeg - 1) $ ts of
        [] -> Nothing
        (t : _) -> t

-- | For a single track, the interpolated fix for each zone tagged.
newtype TrackTag =
    TrackTag
        { zonesTag :: [Maybe Fix]
        -- ^ The interpolated fix tagging each made zone.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | Associates a pilot with the zones they tag for a single task.
data PilotTrackTag =
    PilotTrackTag
        Pilot
        (Maybe TrackTag)
        -- ^ The tags should be Just if the pilot launched.
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance FieldOrdering Tagging where
    fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        ("timing", _) -> LT
        ("tagging", _) -> GT

        ("fix", _) -> LT
        ("time", "fix") -> GT
        ("time", _) -> LT
        ("lat", "fix") -> GT
        ("lat", "time") -> GT
        ("lat", _) -> LT
        ("lng", _) -> GT

        ("zonesSum", _) -> LT
        ("zonesFirst", "zonesSum") -> GT
        ("zonesFirst", _) -> LT
        ("zonesLast", "zonesSum") -> GT
        ("zonesLast", "zonesFirst") -> GT
        ("zonesLast", _) -> LT

        ("zonesRankTime", "zonesSum") -> GT
        ("zonesRankTime", "zonesFirst") -> GT
        ("zonesRankTime", "zonesLast") -> GT
        ("zonesRankTime", _) -> LT
        ("zonesRankPilot", _) -> GT
        _ -> compare a b

