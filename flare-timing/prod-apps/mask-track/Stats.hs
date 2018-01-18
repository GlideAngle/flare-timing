{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Stats (FlightStats(..), DashPathInputs(..), nullStats) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Track.Distance (TrackDistance(..), Land)
import Flight.Mask (RaceSections(..))
import Flight.Comp.Distance (DashPathInputs(..))
import Flight.Score (PilotTime(..), PositionAtEss(..))

data FlightStats =
    FlightStats
        { statTimeRank
            :: Maybe (PilotTime (Quantity Double [u| h |]), PositionAtEss)
        , statLand :: Maybe (TrackDistance Land)
        , statDash :: DashPathInputs
        }

nullStats :: FlightStats
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
