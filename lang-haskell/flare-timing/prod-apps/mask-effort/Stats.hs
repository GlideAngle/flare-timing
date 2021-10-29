module Stats
    ( TimeStats(..)
    , FlightStats(..)
    , nullStats
    ) where

import Data.Time.Clock (UTCTime)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Track.Distance (TrackDistance(..), Effort)
import "flight-gap-allot" Flight.Score (PilotTime(..), ArrivalPlacing(..))

data TimeStats =
    TimeStats
        { ssTime :: PilotTime (Quantity Double [u| h |])
        -- ^ The time taken from the start.
        , gsTime :: PilotTime (Quantity Double [u| h |])
        -- ^ The time taken from the start gate.
        , esMark :: UTCTime
        -- ^ The time the pilot arrived at the end of the speed section.
        , positionAtEss :: Maybe ArrivalPlacing
        }

instance Show TimeStats where
    show TimeStats{..} = show esMark

data FlightStats k =
    FlightStats
        { statTimeRank :: Maybe TimeStats
        , statEffort :: Maybe (TrackDistance Effort)
        }

instance Show (FlightStats k) where
    show FlightStats{..} = show statTimeRank

nullStats :: FlightStats k
nullStats =
    FlightStats
        { statTimeRank = Nothing
        , statEffort = Nothing
        }
