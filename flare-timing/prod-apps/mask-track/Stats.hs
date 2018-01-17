{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Stats (FlightStats(..), DashPathInputs(..), nullStats) where

import Data.Time.Clock (UTCTime)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Kml (MarkedFixes(..))
import Flight.Comp (Task(..))
import Flight.Track.Mask (TrackDistance(..), Land)
import Flight.Mask (FlyCut(..), Ticked, RaceSections(..))
import Flight.Score (PilotTime(..), PositionAtEss(..))

data FlightStats =
    FlightStats
        { statTimeRank
            :: Maybe (PilotTime (Quantity Double [u| h |]), PositionAtEss)
        , statLand :: Maybe (TrackDistance Land)
        , statDash :: DashPathInputs
        }

data DashPathInputs =
    DashPathInputs
        { dashTask :: Maybe Task
        , dashTicked :: Ticked
        , dashFlyCut :: Maybe (FlyCut UTCTime MarkedFixes)
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
