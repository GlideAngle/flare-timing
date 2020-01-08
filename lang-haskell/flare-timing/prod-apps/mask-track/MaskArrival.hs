{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
module MaskArrival (maskArrival, arrivalsByRank, arrivalsByTime) where

import Data.Time.Clock (UTCTime)
import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Control.Monad (join)

import Flight.Comp (Pilot(..))
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Speed (pilotArrivalLag)
import Flight.Track.Mask (MaskingArrival(..))
import Flight.Score
    ( PilotsAtEss(..), ArrivalPlacing(..), ArrivalLag(..)
    , arrivalRankFraction, arrivalTimeFraction
    )
import Stats (TimeStats(..), FlightStats(..))

arrivalsByRank :: [(Pilot, FlightStats k)] -> [(Pilot, TrackArrival)]
arrivalsByRank xs =
    sortOn (rank . snd) $ (fmap . fmap) f ys'
    where
        ys :: [(Pilot, (ArrivalPlacing, UTCTime))]
        ys =
            catMaybes
            $ (\(p, FlightStats{..}) -> do
                pos <- join $ positionAtEss <$> statTimeRank
                esT <- esMark <$> statTimeRank
                return (p, (pos, esT)))
            <$> xs

        pilots :: PilotsAtEss
        pilots = PilotsAtEss . toInteger $ length ys

        minT = minimum $ snd . snd <$> ys

        ys' =
            (\(p, (n, t)) ->
                let lag = ArrivalLag $ pilotArrivalLag minT t
                in (p, (n, lag)))
            <$> ys

        f (position, tm) =
            TrackArrival
                { rank = position
                , lag = tm
                , frac = arrivalRankFraction pilots position
                }

arrivalsByTime :: [(Pilot, FlightStats k)] -> [(Pilot, TrackArrival)]
arrivalsByTime xs =
    sortOn (rank . snd) $ (fmap . fmap) f ys'
    where
        ys :: [(Pilot, (ArrivalPlacing, UTCTime))]
        ys =
            catMaybes
            $ (\(p, FlightStats{..}) -> do
                pos <- join $ positionAtEss <$> statTimeRank
                esT <- esMark <$> statTimeRank
                return (p, (pos, esT)))
            <$> xs

        pilots :: PilotsAtEss
        pilots = PilotsAtEss . toInteger $ length ys

        minT = minimum $ snd . snd <$> ys

        ys' =
            (\(p, (n, t)) ->
                let lag = ArrivalLag $ pilotArrivalLag minT t
                in (p, (n, lag)))
            <$> ys

        f (pEss, tm) =
            TrackArrival
                { rank = pEss
                , lag = tm
                , frac = arrivalTimeFraction pilots tm
                }

maskArrival :: [[(Pilot, TrackArrival)]] -> MaskingArrival
maskArrival as =
    MaskingArrival
        { pilotsAtEss = PilotsAtEss . toInteger . length <$> as
        , arrivalRank = as
        }
