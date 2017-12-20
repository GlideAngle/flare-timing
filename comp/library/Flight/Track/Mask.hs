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
    , TrackBestDistance(..)
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
        , arrival :: [[(Pilot, TrackArrival)]]
        , speed :: [[(Pilot, TrackSpeed)]]
        , distance :: [[(Pilot, TrackBestDistance)]]
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

data TrackBestDistance =
    TrackBestDistance
        { best :: TrackDistance
        -- ^ The best distance achieved.
        , last :: TrackDistance
        -- ^ The distance at landing if landed out.
        }
        deriving (Eq, Ord, Show, Generic)

instance ToJSON TrackBestDistance
instance FromJSON TrackBestDistance

data TrackDistance =
    TrackDistance
        { togo :: Maybe Double
        -- ^ The shortest distance to goal of any fix in the track.
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

        ("arrival", "pilotsAtEss") -> GT
        ("arrival", "bestTime") -> GT
        ("arrival", _) -> LT

        ("speed", "pilotsAtEss") -> GT
        ("speed", "bestTime") -> GT
        ("speed", "arrival") -> GT
        ("speed", _) -> LT

        ("distance", _) -> GT

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

