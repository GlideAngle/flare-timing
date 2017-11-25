{-# LANGUAGE DeriveGeneric #-}

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
    , TrackMask(..)
    , PilotTrackMask(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Flight.Pilot (Pilot(..))

-- | For each task, the masking for that task.
newtype Masking =
    Masking { masking :: [[PilotTrackMask]] }
    deriving (Show, Generic)

instance ToJSON Masking
instance FromJSON Masking

data TrackMask =
    TrackMask
        { madeGoal :: Bool
        -- ^ Was goal made.
        , arrivalRank :: Maybe Int
        -- ^ Rank arrival time.
        , timeToGoal :: Maybe Double
        -- ^ How long did this pilot take to complete the course.
        , distanceToGoal :: Maybe Double
        -- ^ The shortest distance to goal of any fix in the track.
        , distanceMade :: Maybe Double
        -- ^ The task distance minus the distance to goal.
        }
   deriving (Show, Generic)

instance ToJSON TrackMask
instance FromJSON TrackMask

-- | Associates a pilot with a flight summary (mask) for a single task.
data PilotTrackMask =
    PilotTrackMask
        Pilot
        (Maybe TrackMask)
        -- ^ The task summary (mask) should be Just if the pilot launched.
    deriving (Show, Generic)

instance ToJSON PilotTrackMask
instance FromJSON PilotTrackMask
