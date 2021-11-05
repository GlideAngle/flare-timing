{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Flight.Track.Tag
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks tagging task control zones.
-}
module Flight.Track.Tag
    ( TaskTagging(..)
    , CompTagging(..)
    , TrackTime(..)
    , TrackTag(..)
    , PilotTrackTag(..)
    , ZonesFirstTag(..)
    , ZonesLastTag(..)
    , firstLead
    , firstStart
    , lastStarting
    , lastArrival
    , timed
    , starting
    , tagTimes
    , mkCompTagZone, unMkCompTagZone
    ) where

import Data.Maybe (listToMaybe, fromMaybe, catMaybes)
import Data.String (IsString())
import Data.Ord (Down(Down))
import Data.List (transpose, sortOn)
import Data.Time.Clock (UTCTime)
import Control.Monad (join)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Zone.SpeedSection (SpeedSection)
import "flight-gap-allot" Flight.Score (Pilot(..))
import Flight.Comp (FirstLead(..), FirstStart(..), LastArrival(..))
import Flight.Track.Cross (InterpolatedFix(..), ZoneTag(..))
import Flight.Field (FieldOrdering(..))

data TaskTagging =
    TaskTagging
        { timing :: TrackTime
          -- ^ For each made zone, the first and last tag.
        , tagging :: [PilotTrackTag]
          -- ^ For each made zone, the tag.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | For each task, the timing and tagging for that task.
data CompTagging =
    CompTagging
        { timing :: [TrackTime]
          -- ^ For each made zone, the first and last tag.
        , tagging :: [[PilotTrackTag]]
          -- ^ For each made zone, the tag.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

mkCompTagZone :: [TaskTagging] -> CompTagging
mkCompTagZone ts =
    uncurry CompTagging $ unzip
    [ (i, g)
    | TaskTagging{timing = i, tagging = g} <- ts
    ]

unMkCompTagZone :: CompTagging -> [TaskTagging]
unMkCompTagZone CompTagging{timing = is, tagging = gs} = zipWith TaskTagging is gs

-- | The first tagging of each zone.
newtype ZonesFirstTag = ZonesFirstTag [Maybe UTCTime]
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | The last tagging of each zone.
newtype ZonesLastTag = ZonesLastTag [Maybe UTCTime]
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (ToJSON, FromJSON)

-- | The timing and tagging for a single task.
data TrackTime =
    TrackTime
        { zonesSum :: [Int]
        -- ^ For each zone, the number of pilots tagging the zone.
        , zonesFirst :: ZonesFirstTag
        -- ^ For each zone, the time of the first tag.
        , zonesLast :: ZonesLastTag
        -- ^ For each zone, the time of the last tag.
        , zonesRankTime :: [[UTCTime]]
        -- ^ For each zone, the ordered times of each tag.
        , zonesRankPilot :: [[Pilot]]
        -- ^ For each zone, the ordered pilots of each tag.
        , lastLanding :: Maybe UTCTime
        -- ^ For the task, the time of the last landing of any pilot.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | The time of the start given tagging times for a single pilot for each zone.
starting :: SpeedSection -> [Maybe UTCTime] -> Maybe UTCTime
starting ss tags =
    case drop (maybe 1 fst ss - 1) tags of
        [] -> Nothing
        t : _ -> t

-- | The time of the last start and pilots starting at that time.
lastStarting :: SpeedSection -> TrackTime -> (Maybe UTCTime, [Pilot])
lastStarting ss TrackTime{zonesLast = ZonesLastTag tags, zonesRankTime, zonesRankPilot} =
    case drop (maybe 1 fst ss - 1) (zip3 tags zonesRankTime zonesRankPilot) of
        [] -> (Nothing, [])
        (Nothing, _, _) : _ -> (Nothing, [])
        (Just tag, ts, ps) : _ -> (Just tag, fmap snd . filter ((==) tag . fst) $ zip ts ps)

timed
    :: [PilotTrackTag]
    -> [Maybe UTCTime] -- ^ The end of each pilot's flying time.
    -> TrackTime
timed xs ys =
    TrackTime
        { zonesSum = length <$> rankTime
        , zonesFirst = ZonesFirstTag $ firstTag <$> zs'
        , zonesLast = ZonesLastTag $ lastTag <$> zs'
        , zonesRankTime = rankTime
        , zonesRankPilot = (fmap . fmap) fst rs'
        , lastLanding = down
        }
    where
        down =
            case catMaybes ys of
                [] -> Nothing
                ts ->
                    case sortOn Down ts of
                        [] -> Nothing
                        (t : _) -> Just t

        zs :: [[Maybe UTCTime]]
        zs = fromMaybe [] . tagTimes <$> xs

        zs' :: [[Maybe UTCTime]]
        zs' = transpose zs

        rs :: [[Maybe (Pilot, UTCTime)]]
        rs = transpose $ rankByTag xs

        rs' :: [[(Pilot, UTCTime)]]
        rs' = sortOnTag <$> rs

        rankTime = (fmap . fmap) snd rs'

sortOnTag :: forall a. [Maybe (a, UTCTime)] -> [(a, UTCTime)]
sortOnTag xs = sortOn snd $ catMaybes xs

firstTag :: [Maybe UTCTime] -> Maybe UTCTime
firstTag xs =
    if null ys then Nothing else Just $ minimum ys
    where
        ys = catMaybes xs

lastTag :: [Maybe UTCTime] -> Maybe UTCTime
lastTag xs =
    if null ys then Nothing else Just $ maximum ys
    where
        ys = catMaybes xs

-- | Gets the pilots zone tag times.
tagTimes :: PilotTrackTag -> Maybe [Maybe UTCTime]
tagTimes (PilotTrackTag _ Nothing) = Nothing
tagTimes (PilotTrackTag _ (Just xs)) =
    Just $ fmap (time . inter) <$> zonesTag xs

-- | Rank the pilots tagging each zone in a single task.
rankByTag
    :: [PilotTrackTag]
    -- ^ The list of pilots flying the task and the zones they tagged.
    -> [[Maybe (Pilot, UTCTime)]]
    -- ^ For each zone in the task, the sorted list of tag ordered pairs of
    -- pilots and their tag times.
rankByTag xs =
    (fmap . fmap) g zss
    where
        -- A list of pilots and maybe their tagged zones.
        ys :: [(Pilot, Maybe [Maybe UTCTime])]
        ys = (\t@(PilotTrackTag p _) -> (p, tagTimes t)) <$> xs

        f :: (Pilot, Maybe [Maybe UTCTime]) -> Maybe [(Pilot, Maybe UTCTime)]
        f (p, ts) = do
            ts' <- ts
            return $ (,) p <$> ts'

        -- For each zone, an unsorted list of pilots.
        zss :: [[(Pilot, Maybe UTCTime)]]
        zss = catMaybes $ f <$> ys

        -- Associate the pilot with each zone.
        g :: (Pilot, Maybe UTCTime) -> Maybe (Pilot, UTCTime)
        g (p, t) = do
            t' <- t
            return (p, t')

firstLead :: SpeedSection -> ZonesFirstTag -> Maybe FirstLead
firstLead _ (ZonesFirstTag []) = Nothing
firstLead Nothing (ZonesFirstTag (t : _)) = FirstLead <$> t
firstLead (Just (leg, _)) (ZonesFirstTag ts) =
    FirstLead <$>
    case drop (leg - 1) ts of
        [] -> Nothing
        (t : _) -> t

firstStart :: SpeedSection -> UTCTime -> ZonesFirstTag -> Maybe FirstStart
firstStart _ _ (ZonesFirstTag []) = Nothing
firstStart speedSection startTime (ZonesFirstTag times) =
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

lastArrival :: SpeedSection -> ZonesLastTag -> Maybe LastArrival
lastArrival _ (ZonesLastTag []) = Nothing
lastArrival Nothing (ZonesLastTag (t : _)) = LastArrival <$> t
lastArrival (Just (_, lastRaceLeg)) (ZonesLastTag ts) =
    LastArrival <$> (join . listToMaybe $ drop (lastRaceLeg - 1) ts)

-- | For a single track, the interpolated fix for each zone tagged.
newtype TrackTag =
    TrackTag
        { zonesTag :: [Maybe ZoneTag]
        -- ^ The interpolated fix tagging each made zone.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Associates a pilot with the zones they tag for a single task.
data PilotTrackTag =
    PilotTrackTag
        Pilot
        (Maybe TrackTag)
        -- ^ The tags should be Just if the pilot launched.
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance FieldOrdering TaskTagging where
    fieldOrder _ = cmp

instance FieldOrdering CompTagging where
    fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        ("inter", _) -> LT
        ("cross", _) -> GT

        ("fixFrac", _) -> LT
        (_, "fixFrac") -> GT

        ("timing", _) -> LT
        ("tagging", _) -> GT

        ("fix", _) -> LT
        ("time", "fix") -> GT
        ("time", _) -> LT
        ("lat", "fix") -> GT
        ("lat", "time") -> GT
        ("lat", _) -> LT

        ("lng", "alt") -> LT
        ("lng", _) -> GT
        ("alt", _) -> GT

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

        ("zonesRankPilot", "lastLanding") -> LT
        ("zonesRankPilot", _) -> GT

        ("lastLanding", _) -> GT

        _ -> compare a b

