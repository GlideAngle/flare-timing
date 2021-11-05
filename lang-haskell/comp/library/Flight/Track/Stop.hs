{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Flight.Track.Stop
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks tagging task control zones for stopped tasks taking consideration of
a restricted time window for scoring.
-}
module Flight.Track.Stop
    ( RetroActive(..)
    , TaskFraming(..)
    , CompFraming(..)
    , StopWindow(..)
    , TrackScoredSection(..)
    , TrackRacingGateSection(..)
    , TrackRacingStartSection(..)
    , StopFraming(..)
    , tardyElapsed
    , tardyGate
    , stopClipByDuration
    , stopClipByGate
    , endOfScored
    , effectiveTagging
    , mkCompPegFrame, unMkCompPegFrame
    ) where

import Prelude hiding (unzip)
import Data.List.NonEmpty (nonEmpty, unzip)
import Data.Maybe (listToMaybe, isJust)
import Data.List (sort, unzip4)
import Data.String (IsString())
import Data.Time.Clock (UTCTime, addUTCTime)
import Control.Monad (join)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Zone.SpeedSection (SpeedSection)
import Flight.Clip (FlyingSection)
import "flight-gap-allot" Flight.Score (Pilot(..))
import Flight.Comp (LastStart(..), StartGate(..))
import Flight.Track.Cross (Seconds(..))
import Flight.Track.Tag (CompTagging(..), TrackTime(..), PilotTrackTag(..), ZonesLastTag(..))
import Flight.Track.Speed (startGateTaken)
import Flight.Field (FieldOrdering(..))
import Flight.Track.Curry (uncurry4)

-- | For a stopped task, this is the time the task is scored until, the
-- announced stop time wound back by the score back time.
newtype RetroActive = RetroActive UTCTime
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data StopWindow =
    StopWindow
        { lastStartTime :: Maybe UTCTime
        -- ^ The start time of the pilot or pilots last to start.
        , lastStarters :: [Pilot]
        -- ^ The pilot or pilots last to start in an elapsed time race or
        -- a race to goal task with multiple start gates. For race to goal
        -- tasks with a single start gate @lastStarters@ will be an empty list.
        , stopWindowTimes :: FlyingSection UTCTime
        -- ^ The scored window for a stopped task as a time range. For an
        -- elapsed time race or a race to goal task with only one start gate
        -- this will be the range from the start until the retroactive stop
        -- time. For race to goal task with multiple start gates this will be
        -- the time available racing for the last pilot or pilots to start.
        , stopWindowSeconds :: Seconds
        -- ^ The width of the time window.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | For a single track, the scored section.
data TrackScoredSection =
    TrackScoredSection
        { scoredFixes :: FlyingSection Int
        -- ^ The scored section as indices into the list of fixes.
        , scoredSeconds :: FlyingSection Seconds
        -- ^ The scored section as second offsets from the first fix.
        , scoredTimes :: FlyingSection UTCTime
        -- ^ The scored section as a time range.
        , scoredWindowSeconds :: Maybe Seconds
        -- ^ The width of the time window of the scoring section.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | For a single track, the racing section from the start gate taken.
data TrackRacingGateSection =
    TrackRacingGateSection
        { racingGateFixes :: FlyingSection Int
        -- ^ The racing section from the opening of the gate taken as indices into the list of fixes.
        , racingGateSeconds :: FlyingSection Seconds
        -- ^ The racing section from the opening of the gate taken as second offsets from the first fix.
        , racingGateTimes :: FlyingSection UTCTime
        -- ^ The racing section from the opening of the gate taken as a time range.
        , racingGateWindowSeconds :: Maybe Seconds
        -- ^ The width of the time window from opening of the start gate taken to the end of the scoring window.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | For a single track, the racing section from the actual starting time. This
-- will be at or after a start gate opening time except when jumping the gun. A
-- pilot that jumps the gun may start before the opening of the first start
-- gate.
data TrackRacingStartSection =
    TrackRacingStartSection
        { racingStartFixes :: FlyingSection Int
        -- ^ The racing section from the starting time as indices into the list of fixes.
        , racingStartSeconds :: FlyingSection Seconds
        -- ^ The racing section from the starting time as second offsets from the first fix.
        , racingStartTimes :: FlyingSection UTCTime
        -- ^ The racing section from the starting time as a time range.
        , racingStartWindowSeconds :: Maybe Seconds
        -- ^ The width of the time window from opening of the starting time to the end of the scoring window.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data StopFraming =
    StopFraming
        { stopScored :: Maybe TrackScoredSection
        , stopRacingGate :: Maybe TrackRacingGateSection
        , stopRacingStart :: Maybe TrackRacingStartSection
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | The timing and tagging that includes restriction of time if the task was
-- stopped.
data TaskFraming =
    TaskFraming
        { stopWindow :: Maybe StopWindow
        -- ^ The scored time window for a stopped task.
        , stopFlying :: [(Pilot, StopFraming)]
        -- ^ The pilots' flying section that is scored.
        , timing :: TrackTime
          -- ^ For each made zone, the first and last tag.
        , tagging :: [PilotTrackTag]
          -- ^ For each made zone, the tag.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | For each task, the timing and tagging for that task that includes
-- restriction of time if the task was stopped.
data CompFraming =
    CompFraming
        { stopWindow :: [Maybe StopWindow]
        -- ^ The scored time window for a stopped task.
        , stopFlying :: [[(Pilot, StopFraming)]]
        -- ^ For each task, the pilots' flying section that is scored.
        , timing :: [TrackTime]
          -- ^ For each made zone, the first and last tag.
        , tagging :: [[PilotTrackTag]]
          -- ^ For each made zone, the tag.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

mkCompPegFrame :: [TaskFraming] -> CompFraming
mkCompPegFrame ts =
    uncurry4 CompFraming $ unzip4
    [ (w, f, i, g)
    | TaskFraming{stopWindow = w, stopFlying = f, timing = i, tagging = g} <- ts
    ]

unMkCompPegFrame :: CompFraming -> [TaskFraming]
unMkCompPegFrame CompFraming{stopWindow = ws, stopFlying = fs, timing = is, tagging = gs} =
    [TaskFraming w f i g | w <- ws | f <- fs | i <- is | g <- gs]

effectiveTagging :: CompTagging -> CompFraming -> CompTagging
effectiveTagging
    CompTagging{timing = tssTag, tagging = gssTag}
    CompFraming{timing = tssStop, tagging = gssStop, stopWindow} =
        uncurry CompTagging . unzip $
        [ if isJust sw then tsStop else tsTag
        | sw <- stopWindow
        | tsTag <- zip tssTag gssTag
        | tsStop <- zip tssStop gssStop
        ]

endOfScored :: Maybe TrackScoredSection -> Maybe UTCTime
endOfScored = ((fmap snd . scoredTimes) =<<)

-- | Find the last crossing of the start of an elapsed time task.
tardyElapsed :: SpeedSection -> ZonesLastTag -> Maybe LastStart
tardyElapsed _ (ZonesLastTag []) = Nothing
tardyElapsed Nothing (ZonesLastTag (t : _)) = LastStart <$> t
tardyElapsed (Just (firstRaceLeg, _)) (ZonesLastTag ts) =
    LastStart <$> (join . listToMaybe $ drop (firstRaceLeg - 1) ts)

-- | Find the last start of a race task with start gates.
tardyGate :: [StartGate] -> SpeedSection -> [[UTCTime]] -> [[Pilot]] -> Maybe StartGate
tardyGate _ _ [] _ = Nothing
tardyGate _ _ _ [] = Nothing
tardyGate [g] _ _ _ = Just g
tardyGate gs ss ts _ = do
    starts <- listToMaybe $ maybe ts (\(firstRaceLeg, _) -> drop (firstRaceLeg - 1) ts) ss
    gs' <- nonEmpty gs
    lastStart <- listToMaybe . reverse $ sort starts
    return . snd $ startGateTaken gs' lastStart

-- | The scoring time window can be limited when a task was stopped. Find the
-- scored time window. This is the flying time window truncated to only so many
-- seconds after the start taken.
-- TODO: Add a parameter for the start time to stopClipByDuration.
stopClipByDuration :: Seconds -> FlyingSection UTCTime -> FlyingSection UTCTime
stopClipByDuration (Seconds n) x = do
    (s, e) <- x
    return (s, min e $ fromIntegral n `addUTCTime` s)

-- | The scoring time window can be limited when a task was stopped. Find the
-- start gate taken and the scored time window. This is the flying time window
-- truncated to only so many seconds after the start gate taken.
stopClipByGate :: Seconds -> [StartGate] -> FlyingSection UTCTime -> (Maybe StartGate, FlyingSection UTCTime)
stopClipByGate (Seconds n) [sg@(StartGate g)] x = unzip $ do
    (s, e) <- x
    return (sg, (s, min e $ fromIntegral n `addUTCTime` g))
stopClipByGate (Seconds n) gs x = unzip $ do
    (s, e) <- x
    gs' <- nonEmpty gs
    let sg@(StartGate g) = snd $ startGateTaken gs' s
    return (sg, (s, min e $ fromIntegral n `addUTCTime` g))

instance FieldOrdering TaskFraming where
    fieldOrder _ = cmp

instance FieldOrdering CompFraming where
    fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        ("stopWindow", _) -> LT

        ("stopFlying", "stopWindow") -> GT
        ("stopFlying", _) -> LT

        ("timing", "stopWindow") -> GT
        ("timing", "stopFlying") -> GT
        ("timing", _) -> LT

        ("tagging", _) -> GT


        ("lastStarters", _) -> LT

        ("stopWindowTimes", "lastStarters") -> GT
        ("stopWindowTimes", _) -> LT

        ("stopWindowSeconds", "lastStarters") -> GT
        ("stopWindowSeconds", "stopWindowTimes") -> GT
        ("stopWindowSeconds", _) -> LT

        ("inter", _) -> LT
        ("cross", _) -> GT

        ("fixFrac", _) -> LT
        (_, "fixFrac") -> GT

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

