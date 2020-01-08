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
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Score (ArrivalPlacing(..), ArrivalFraction(..), ArrivalLag(..))

-- ^ If arrived at goal then arrival rank and fraction.
data TrackArrival =
    TrackArrival
        { rank :: ArrivalPlacing
        , lag :: ArrivalLag (Quantity Double [u| h |])
        , frac :: ArrivalFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
