{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
module MaskArrival (maskArrival, arrivalsByRank, arrivalsByTime) where

import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Control.Monad (join)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Comp (Pilot(..))
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Mask (MaskingArrival(..))
import Flight.Score
    ( PilotsAtEss(..), ArrivalPlacing(..), PilotTime(..), ArrivalTime(..)
    , arrivalRankFraction, arrivalTimeFraction
    )
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

arrivalsByTime :: [(Pilot, FlightStats k)] -> [(Pilot, TrackArrival)]
arrivalsByTime xs =
    sortOn (rank . snd) $ (fmap . fmap) (f $ ArrivalTime minT) ys
    where
        ys :: [(Pilot, (ArrivalPlacing, Quantity Double [u| h |]))]
        ys =
            catMaybes
            $ (\(p, FlightStats{..}) -> do
                pos <- join $ positionAtEss <$> statTimeRank
                PilotTime gsT <- gsTime <$> statTimeRank
                return (p, (pos, gsT)))
            <$> xs

        pilots :: PilotsAtEss
        pilots = PilotsAtEss . toInteger $ length ys

        minT = minimum $ snd . snd <$> ys

        f t0 (pEss, tm) =
            TrackArrival
                { rank = pEss
                , frac = arrivalTimeFraction pilots t0 (ArrivalTime tm)
                }

maskArrival :: [[(Pilot, TrackArrival)]] -> MaskingArrival
maskArrival as =
    MaskingArrival
        { pilotsAtEss = PilotsAtEss . toInteger . length <$> as
        , arrivalRank = as
        }
