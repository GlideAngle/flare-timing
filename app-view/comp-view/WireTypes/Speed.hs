module WireTypes.Speed
    ( SpeedFraction(..)
    , TrackSpeed(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))

import WireTypes.Point (PilotTime(..))

newtype SpeedFraction = SpeedFraction Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data TrackSpeed =
    TrackSpeed
        { time :: PilotTime String
        , frac :: SpeedFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)
