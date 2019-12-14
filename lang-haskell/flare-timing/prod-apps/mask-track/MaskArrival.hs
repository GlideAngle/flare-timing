{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
module MaskArrival (maskArrival, arrivalsByRank) where

import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Control.Monad (join)

import Flight.Comp (Pilot(..))
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Mask (MaskingArrival(..))
import Flight.Score (PilotsAtEss(..), ArrivalPlacing(..), arrivalRankFraction)
import Stats (TimeStats(..), FlightStats(..))

arrivalsByRank :: [(Pilot, FlightStats k)] -> [(Pilot, TrackArrival)]
arrivalsByRank xs =
    sortOn (rank . snd) $ (fmap . fmap) f ys
    where
        ys :: [(Pilot, ArrivalPlacing)]
        ys =
            catMaybes
            $ (\(p, FlightStats{..}) -> (p,) <$> (join $ positionAtEss <$> statTimeRank))
            <$> xs

        pilots :: PilotsAtEss
        pilots = PilotsAtEss . toInteger $ length ys

        f position =
            TrackArrival
                { rank = position
                , frac = arrivalRankFraction pilots position
                }

maskArrival :: [[(Pilot, TrackArrival)]] -> MaskingArrival
maskArrival as =
    MaskingArrival
        { pilotsAtEss = PilotsAtEss . toInteger . length <$> as
        , arrivalRank = as
        }
