{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

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
    , Nigh
    , Land
    ) where

import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
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
    )
import Data.Aeson.ViaScientific (ViaScientific(..))
import Flight.Field (FieldOrdering(..))

type Nigh = TrackLine
type Land = Double

-- | For each task, the masking for that task.
data Masking =
    Masking
        { pilotsAtEss :: [PilotsAtEss]
        -- ^ For each task, the number of pilots at goal.
        , bestTime :: [Maybe (ViaScientific BestTime)]
        -- ^ For each task, the best time.
        , taskDistance :: [Maybe Double]
        -- ^ For each task, the task distance.
        , bestDistance :: [Maybe Double]
        -- ^ For each task, the best distance made.
        , minLead :: [Maybe (ViaScientific LeadingCoefficient)]
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

-- ^ If arrived at goal then speed fraction.
data TrackSpeed =
    TrackSpeed
        { time :: ViaScientific PilotTime
        , frac :: ViaScientific SpeedFraction
        }
        deriving (Eq, Ord, Show, Generic)

instance ToJSON TrackSpeed
instance FromJSON TrackSpeed

-- ^ If arrived at goal then arrival rank and fraction.
data TrackArrival =
    TrackArrival
        { rank :: PositionAtEss
        , frac :: ViaScientific ArrivalFraction
        }
        deriving (Eq, Ord, Show, Generic)

instance ToJSON TrackArrival
instance FromJSON TrackArrival

data TrackLead =
    TrackLead
        { coef :: ViaScientific LeadingCoefficient
        , frac :: ViaScientific LeadingFraction
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
        ("pilotsAtEss", _) -> LT

        ("best", _) -> LT
        ("last", _) -> GT

        ("bestTime", "pilotsAtEss") -> GT
        ("bestTime", _) -> LT

        ("taskDistance", "pilotsAtEss") -> GT
        ("taskDistance", "bestTime") -> GT
        ("taskDistance", _) -> LT

        ("bestDistance", "pilotsAtEss") -> GT
        ("bestDistance", "bestTime") -> GT
        ("bestDistance", "taskDistance") -> GT
        ("bestDistance", _) -> LT

        ("minLead", "pilotsAtEss") -> GT
        ("minLead", "bestTime") -> GT
        ("minLead", "taskDistance") -> GT
        ("minLead", "bestDistance") -> GT
        ("minLead", _) -> LT

        ("lead", "pilotsAtEss") -> GT
        ("lead", "bestTime") -> GT
        ("lead", "taskDistance") -> GT
        ("lead", "bestDistance") -> GT
        ("lead", "minLead") -> GT
        ("lead", _) -> LT

        ("arrival", "pilotsAtEss") -> GT
        ("arrival", "bestTime") -> GT
        ("arrival", "taskDistance") -> GT
        ("arrival", "bestDistance") -> GT
        ("arrival", "minLead") -> GT
        ("arrival", "lead") -> GT
        ("arrival", _) -> LT

        ("speed", "pilotsAtEss") -> GT
        ("speed", "bestTime") -> GT
        ("speed", "taskDistance") -> GT
        ("speed", "bestDistance") -> GT
        ("speed", "minLead") -> GT
        ("speed", "lead") -> GT
        ("speed", "arrival") -> GT
        ("speed", _) -> LT

        ("nigh", "pilotsAtEss") -> GT
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

