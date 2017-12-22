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
    ) where

import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Flight.Pilot (Pilot(..))
import Flight.Score
    ( PilotsAtEss(..)
    , PositionAtEss(..)
    , ArrivalFraction(..)
    , SpeedFraction(..)
    , BestTime(..)
    , PilotTime(..)
    )
import Data.Aeson.ViaScientific (ViaScientific(..))
import Flight.Field (FieldOrdering(..))

-- | For each task, the masking for that task.
data Masking =
    Masking
        { pilotsAtEss :: [PilotsAtEss]
        , bestTime :: [Maybe (ViaScientific BestTime)]
        , taskDistance :: [Maybe Double]
        , bestDistance :: [Maybe Double]
        , arrival :: [[(Pilot, TrackArrival)]]
        , speed :: [[(Pilot, TrackSpeed)]]
        , nigh :: [[(Pilot, TrackDistance)]]
        , land :: [[(Pilot, TrackDistance)]]
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

data TrackDistance =
    TrackDistance
        { togo :: Maybe Double
        -- ^ The distance to goal.
        , made :: Maybe Double
        -- ^ The task distance minus the distance to goal.
        }
        deriving (Eq, Ord, Show, Generic)

instance ToJSON TrackDistance
instance FromJSON TrackDistance

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

        ("arrival", "pilotsAtEss") -> GT
        ("arrival", "bestTime") -> GT
        ("arrival", "taskDistance") -> GT
        ("arrival", "bestDistance") -> GT
        ("arrival", _) -> LT

        ("speed", "pilotsAtEss") -> GT
        ("speed", "taskDistance") -> GT
        ("speed", "bestDistance") -> GT
        ("speed", "bestTime") -> GT
        ("speed", "arrival") -> GT
        ("speed", _) -> LT

        ("nigh", "pilotsAtEss") -> GT
        ("nigh", "taskDistance") -> GT
        ("nigh", "bestDistance") -> GT
        ("nigh", "bestTime") -> GT
        ("nigh", "arrival") -> GT
        ("nigh", "speed") -> GT
        ("nigh", _) -> LT

        ("land", _) -> GT

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

