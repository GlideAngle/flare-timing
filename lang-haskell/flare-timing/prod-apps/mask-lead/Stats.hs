module Stats
    ( TimeStats(..)
    , FlightStats(..)
    , DashPathInputs(..)
    , nullStats
    , altToAlt
    ) where

import Data.Time.Clock (UTCTime)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (QAlt, Alt(..))
import Flight.Kml (Altitude(..))
import Flight.Track.Distance (TrackDistance(..), Effort)
import Flight.Comp.Distance (DashPathInputs(..))
import "flight-gap-allot" Flight.Score (PilotTime(..), ArrivalPlacing(..))

altToAlt :: Altitude -> QAlt Double [u| m |]
altToAlt (Altitude x) = Alt . MkQuantity . fromIntegral $ x

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
