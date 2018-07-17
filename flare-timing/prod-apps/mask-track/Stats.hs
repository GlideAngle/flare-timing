module Stats
    ( TimeStats(..)
    , FlightStats(..)
    , DashPathInputs(..)
    , nullStats
    ) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Track.Distance (TrackDistance(..), Land)
import Flight.Mask (RaceSections(..))
import Flight.Comp.Distance (DashPathInputs(..))
import Flight.Score (PilotTime(..), PositionAtEss(..))

data TimeStats =
    TimeStats
        { ssTime :: PilotTime (Quantity Double [u| h |])
          -- ^ The time taken from the start.
        , gsTime :: PilotTime (Quantity Double [u| h |])
          -- ^ The time taken from the start gate.
        , positionAtEss :: PositionAtEss
        }

data FlightStats k =
    FlightStats
        { statTimeRank :: Maybe TimeStats
        , statLand :: Maybe (TrackDistance Land)
        , statDash :: DashPathInputs k
        }

nullStats :: FlightStats k
nullStats =
    FlightStats
        { statTimeRank = Nothing
        , statLand = Nothing
        , statDash =
            DashPathInputs
                { dashTask = Nothing
                , dashTicked = RaceSections [] [] []
                , dashFlyCut = Nothing
                }
        }
