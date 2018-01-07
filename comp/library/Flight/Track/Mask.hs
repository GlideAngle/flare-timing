{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Flight.Track.Mask
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks masked with task control zones.
-}
module Flight.Track.Mask
    ( Masking(..)
    , TrackDistance(..)
    , TrackArrival(..)
    , TrackSpeed(..)
    , TrackLead(..)
    , RaceTime(..)
    , Nigh
    , Land
    , racing
    ) where

import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Flight.Comp (OpenClose(..), FirstLead(..), FirstStart(..), LastArrival(..))
import Flight.Pilot (Pilot(..))
import Flight.Route (TrackLine(..))
import Flight.Score
    ( PilotsAtEss(..)
    , PositionAtEss(..)
    , ArrivalFraction(..)
    , SpeedFraction(..)
    , BestTime(..)
    , PilotTime(..)
    , LeadingCoefficient(..)
    , LeadingFraction(..)
    , EssTime(..)
    )
import Data.Aeson.Via.Scientific (ViaSci(..))
import Flight.Field (FieldOrdering(..))

type Nigh = TrackLine
type Land = Double

-- | For each task, the masking for that task.
data Masking =
    Masking
        { pilotsAtEss :: [PilotsAtEss]
        -- ^ For each task, the number of pilots at goal.
        , raceTime :: [Maybe RaceTime]
        -- ^ For each task, the time of the last pilot crossing goal.
        , bestTime :: [Maybe (ViaSci BestTime)]
        -- ^ For each task, the best time.
        , taskDistance :: [Maybe Double]
        -- ^ For each task, the task distance.
        , bestDistance :: [Maybe Double]
        -- ^ For each task, the best distance made.
        , minLead :: [Maybe (ViaSci LeadingCoefficient)]
        -- ^ For each task, the minimum of all pilot's leading coefficient.
        , lead :: [[(Pilot, TrackLead)]]
        -- ^ For each task, the rank order of leading and leading fraction.
        , arrival :: [[(Pilot, TrackArrival)]]
        -- ^ For each task, the rank order of arrival at goal and arrival fraction.
        , speed :: [[(Pilot, TrackSpeed)]]
        -- ^ For each task, for each pilot making goal, their time for the
        -- speed section and speed fraction.
        , nigh :: [[(Pilot, TrackDistance Nigh)]]
        -- ^ For each task, the best distance of each pilot landing out.
        , land :: [[(Pilot, TrackDistance Land)]]
        -- ^ For each task, the distance of the landing spot for each pilot
        -- landing out.
        }
        deriving (Eq, Ord, Show, Generic)

instance ToJSON Masking
instance FromJSON Masking

-- | The racing time for the speed section is required for leading points.
data RaceTime =
    RaceTime
        { openTask :: UTCTime
        -- ^ The time of first allowed crossing of the start of the speed section.
        , closeTask :: UTCTime
        -- ^ The time of last allowed crossing of the end of the speed section.
        , firstLead :: Maybe FirstLead
        , firstStart :: Maybe FirstStart
        , lastArrival :: Maybe LastArrival
        , leadArrival :: Maybe (ViaSci EssTime)
        -- ^ When the last pilot arrives at goal, seconds from the time of first lead.
        , leadClose :: Maybe (ViaSci EssTime)
        -- ^ When the task closes, seconds from the time of first lead.
        , tickClose :: Maybe (ViaSci EssTime)
        -- ^ When the task closes, seconds from the time of first race start.
        , openClose :: ViaSci EssTime
        -- ^ Seconds from open to close
        }
        deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

racing
    :: Maybe OpenClose
    -> Maybe FirstLead
    -> Maybe FirstStart
    -> Maybe LastArrival
    -> Maybe RaceTime
racing oc firstLead firstStart lastArrival = do
    OpenClose{open, close} <- oc
    return
        RaceTime
            { openTask = open
            , closeTask = close
            , firstLead = firstLead
            , firstStart = firstStart
            , lastArrival = lastArrival

            , leadArrival = ViaSci . EssTime . toRational <$> do
                FirstLead lead <- firstLead
                LastArrival end <- lastArrival
                return $ end `diffUTCTime` lead
                
            , leadClose = ViaSci . EssTime . toRational <$> do
                FirstLead lead <- firstLead
                return $ close `diffUTCTime` lead

            , tickClose = ViaSci . EssTime . toRational <$> do
                FirstStart start <- firstStart
                return $ close `diffUTCTime` start

            , openClose = ViaSci . EssTime . toRational $
                close `diffUTCTime` open 
            }

-- ^ If arrived at goal then speed fraction.
data TrackSpeed =
    TrackSpeed
        { time :: ViaSci PilotTime
        , frac :: ViaSci SpeedFraction
        }
        deriving (Eq, Ord, Show, Generic)

instance ToJSON TrackSpeed
instance FromJSON TrackSpeed

-- ^ If arrived at goal then arrival rank and fraction.
data TrackArrival =
    TrackArrival
        { rank :: PositionAtEss
        , frac :: ViaSci ArrivalFraction
        }
        deriving (Eq, Ord, Show, Generic)

instance ToJSON TrackArrival
instance FromJSON TrackArrival

data TrackLead =
    TrackLead
        { coef :: ViaSci LeadingCoefficient
        , frac :: ViaSci LeadingFraction
        }
        deriving (Eq, Ord, Show, Generic)

instance ToJSON TrackLead
instance FromJSON TrackLead

data TrackDistance a =
    TrackDistance
        { togo :: Maybe a
        -- ^ The distance to goal.
        , made :: Maybe Double
        -- ^ The task distance minus the distance to goal.
        }
        deriving (Eq, Ord, Show, Generic)

instance (ToJSON a) => ToJSON (TrackDistance a)
instance (FromJSON a) => FromJSON (TrackDistance a)

instance FieldOrdering Masking where
    fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        -- TODO: first start time & last goal time & launched
        ("openTask", _) -> LT

        ("closeTask", "openTask") -> GT
        ("closeTask", _) -> LT

        ("firstStart", "openTask") -> GT
        ("firstStart", "closeTask") -> GT
        ("firstStart", _) -> LT

        ("lastArrival", "openTask") -> GT
        ("lastArrival", "closeTask") -> GT
        ("lastArrival", "firstStart") -> GT
        ("lastArrival", _) -> LT

        ("tickArrival", "openTask") -> GT
        ("tickArrival", "closeTask") -> GT
        ("tickArrival", "firstStart") -> GT
        ("tickArrival", "lastArrival") -> GT
        ("tickArrival", _) -> LT

        ("tickRace", "openTask") -> GT
        ("tickRace", "closeTask") -> GT
        ("tickRace", "firstStart") -> GT
        ("tickRace", "lastArrival") -> GT
        ("tickRace", "tickArrival") -> GT
        ("tickRace", _) -> LT

        ("tickTask", _) -> GT

        ("pilotsAtEss", _) -> LT

        ("raceTime", "pilotsAtEss") -> GT
        ("raceTime", _) -> LT

        ("best", _) -> LT
        ("last", _) -> GT

        ("bestTime", "pilotsAtEss") -> GT
        ("bestTime", _) -> LT

        ("taskDistance", "pilotsAtEss") -> GT
        ("taskDistance", "raceTime") -> GT
        ("taskDistance", "bestTime") -> GT
        ("taskDistance", _) -> LT

        ("bestDistance", "pilotsAtEss") -> GT
        ("bestDistance", "raceTime") -> GT
        ("bestDistance", "bestTime") -> GT
        ("bestDistance", "taskDistance") -> GT
        ("bestDistance", _) -> LT

        ("minLead", "pilotsAtEss") -> GT
        ("minLead", "raceTime") -> GT
        ("minLead", "bestTime") -> GT
        ("minLead", "taskDistance") -> GT
        ("minLead", "bestDistance") -> GT
        ("minLead", _) -> LT

        ("lead", "pilotsAtEss") -> GT
        ("lead", "raceTime") -> GT
        ("lead", "bestTime") -> GT
        ("lead", "taskDistance") -> GT
        ("lead", "bestDistance") -> GT
        ("lead", "minLead") -> GT
        ("lead", _) -> LT

        ("arrival", "pilotsAtEss") -> GT
        ("arrival", "raceTime") -> GT
        ("arrival", "bestTime") -> GT
        ("arrival", "taskDistance") -> GT
        ("arrival", "bestDistance") -> GT
        ("arrival", "minLead") -> GT
        ("arrival", "lead") -> GT
        ("arrival", _) -> LT

        ("speed", "pilotsAtEss") -> GT
        ("speed", "raceTime") -> GT
        ("speed", "bestTime") -> GT
        ("speed", "taskDistance") -> GT
        ("speed", "bestDistance") -> GT
        ("speed", "minLead") -> GT
        ("speed", "lead") -> GT
        ("speed", "arrival") -> GT
        ("speed", _) -> LT

        ("nigh", "pilotsAtEss") -> GT
        ("nigh", "raceTime") -> GT
        ("nigh", "bestTime") -> GT
        ("nigh", "taskDistance") -> GT
        ("nigh", "bestDistance") -> GT
        ("nigh", "minLead") -> GT
        ("nigh", "lead") -> GT
        ("nigh", "arrival") -> GT
        ("nigh", "speed") -> GT
        ("nigh", _) -> LT

        ("land", _) -> GT

        ("coef", _) -> LT
        ("time", _) -> LT
        ("rank", _) -> LT
        ("frac", _) -> GT

        ("madeGoal", _) -> LT
        ("arrivalRank", "madeGoal") -> GT
        ("arrivalRank", _) -> LT
        ("timeToGoal", "madeGoal") -> GT
        ("timeToGoal", "arrivalRank") -> GT
        ("timeToGoal", _) -> LT

        ("togo", _) -> LT
        ("made", _) -> GT
        _ -> compare a b

