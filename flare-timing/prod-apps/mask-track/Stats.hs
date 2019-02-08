module Stats
    ( TimeStats(..)
    , FlightStats(..)
    , DashPathInputs(..)
    , nullStats
    , altToAlt
    ) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (QAlt, Alt(..))
import Flight.Kml (Altitude(..))
import Flight.Track.Distance (TrackDistance(..), Land)
import Flight.Mask (RaceSections(..))
import Flight.Comp.Distance (DashPathInputs(..))
import Flight.Score (PilotTime(..), PositionAtEss(..))

altToAlt :: Altitude -> QAlt Double [u| m |]
altToAlt (Altitude x) = Alt . MkQuantity . fromIntegral $ x

data TimeStats =
    TimeStats
        { ssTime :: PilotTime (Quantity Double [u| h |])
          -- ^ The time taken from the start.
        , gsTime :: PilotTime (Quantity Double [u| h |])
          -- ^ The time taken from the start gate.
        , positionAtEss :: Maybe PositionAtEss
        }

data FlightStats k =
    FlightStats
        { statTimeRank :: Maybe TimeStats
        , statLand :: Maybe (TrackDistance Land)
        , statAlt :: Maybe (QAlt Double [u| m |])
        , statDash :: DashPathInputs k
        }

nullStats :: FlightStats k
nullStats =
    FlightStats
        { statTimeRank = Nothing
        , statLand = Nothing
        , statAlt = Nothing
        , statDash =
            DashPathInputs
                { dashTask = Nothing
                , dashTicked = RaceSections [] [] []
                , dashFlyCut = Nothing
                }
        }
