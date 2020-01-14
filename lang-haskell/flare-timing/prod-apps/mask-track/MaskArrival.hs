{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
module MaskArrival (maskArrival, arrivalInputs) where

import Data.Maybe (catMaybes)

import Flight.Comp (Pilot(..))
import Flight.Track.Arrival (ArrivalInputs, TrackArrival(..))
import Flight.Track.Mask (MaskingArrival(..))
import Flight.Score (PilotsAtEss(..))
import Stats (TimeStats(..), FlightStats(..))

arrivalInputs :: [(Pilot, FlightStats k)] -> ArrivalInputs
arrivalInputs xs =
    catMaybes
    $ (\(p, FlightStats{..}) -> do
        esT <- esMark <$> statTimeRank
        return (p, esT))
    <$> xs


maskArrival :: [[(Pilot, TrackArrival)]] -> MaskingArrival
maskArrival as =
    MaskingArrival
        { pilotsAtEss = PilotsAtEss . toInteger . length <$> as
        , arrivalRank = as
        }
