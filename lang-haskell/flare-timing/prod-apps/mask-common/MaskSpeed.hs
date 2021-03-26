{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module MaskSpeed (maskSpeedBestTime) where

import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Control.Arrow (second)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Comp (Pilot(..))
import Flight.Track.Speed (TrackSpeed(..))
import qualified "flight-gap-allot" Flight.Score as Gap (bestTime')
import "flight-gap-allot" Flight.Score
    (BestTime(..), PilotTime(..), PowerExponent, speedFraction)
import Stats (TimeStats(..), FlightStats(..))

times
    :: PowerExponent
    -> (TimeStats -> PilotTime (Quantity Double [u| h |]))
    -> [(Pilot, FlightStats k)]
    -> Maybe (BestTime (Quantity Double [u| h |]), [(Pilot, TrackSpeed)])
times pe f xs =
    (\ bt -> (bt, sortOn (time . snd) $ second (g bt) <$> ys))
    <$> Gap.bestTime' ts
    where
        ys :: [(Pilot, PilotTime (Quantity Double [u| h |]))]
        ys =
            catMaybes
            $ (\(p, FlightStats{..}) -> (p,) . f <$> statTimeRank)
            <$> xs

        ts :: [PilotTime (Quantity Double [u| h |])]
        ts = snd <$> ys

        g best t =
            TrackSpeed
                { time = t
                , frac = speedFraction pe best t
                }

maskSpeedBestTime
    :: PowerExponent
    -> [[(Pilot, FlightStats k)]]
    -> ([Maybe (BestTime (Quantity Double [u| h |]))])
maskSpeedBestTime pe yss = (fmap . fmap) fst $ times pe gsTime <$> yss
