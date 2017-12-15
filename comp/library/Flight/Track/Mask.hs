{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

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

-- | For each task, the masking for that task.
data Masking =
    Masking
        { pilotsAtEss :: [PilotsAtEss]
        , bestTime :: [Maybe (ViaScientific BestTime)]
        , arrival :: [[(Pilot, TrackArrival)]]
        , speed :: [[(Pilot, TrackSpeed)]]
        , distance :: [[(Pilot, TrackDistance)]]
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
        -- ^ The shortest distance to goal of any fix in the track.
        , made :: Maybe Double
        -- ^ The task distance minus the distance to goal.
        }
        deriving (Eq, Ord, Show, Generic)

instance ToJSON TrackDistance
instance FromJSON TrackDistance
