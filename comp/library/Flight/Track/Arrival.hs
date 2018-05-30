{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Flight.Track.Arrival
Copyright   : (c) Block Scope Limited 2018
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

The arrival standing of a pilot's track in comparison to other pilots.
-}
module Flight.Track.Arrival (TrackArrival(..)) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Score (PositionAtEss(..), ArrivalFraction(..))

-- ^ If arrived at goal then arrival rank and fraction.
data TrackArrival =
    TrackArrival
        { rank :: PositionAtEss
        , frac :: ArrivalFraction
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
