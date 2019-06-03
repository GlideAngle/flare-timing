{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module MaskSpeed (maskSpeed) where

import Data.Maybe (fromMaybe, catMaybes)
import Data.List (sortOn)
import Control.Arrow (second)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (QAlt)
import Flight.Comp (Pilot(..), TaskRouteDistance(..))
import Flight.Track.Mask (MaskingSpeed(..))
import Flight.Track.Speed (TrackSpeed(..))
import qualified Flight.Score as Gap (bestTime')
import Flight.Score (BestTime(..), PilotTime(..), speedFraction)
import Stats (TimeStats(..), FlightStats(..))

landAltitudes :: [(Pilot, FlightStats k)] -> [(Pilot, QAlt Double [u| m |])]
landAltitudes xs =
    catMaybes
    $ fmap (\(p, FlightStats{..}) -> (p,) <$> statAlt) xs

times
    :: (TimeStats -> PilotTime (Quantity Double [u| h |]))
    -> [(Pilot, FlightStats k)]
    -> Maybe (BestTime (Quantity Double [u| h |]), [(Pilot, TrackSpeed)])
times f xs =
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
                , frac = speedFraction best t
                }

maskSpeed
    :: [Maybe TaskRouteDistance]
    -> [[(Pilot, FlightStats k)]]
    -> ([Maybe (BestTime (Quantity Double [u| h |]))], MaskingSpeed)
maskSpeed lsTask yss =
    (gsBestTime,) $
    MaskingSpeed
        { ssBestTime = ssBestTime
        , gsBestTime = gsBestTime
        , taskDistance = lsWholeTask
        , taskSpeedDistance = lsSpeedSubset
        , ssSpeed = fromMaybe [] <$> (fmap . fmap) snd ssVs
        , gsSpeed = fromMaybe [] <$> (fmap . fmap) snd gsVs
        , altStopped = dsAlt
        }
    where
        lsWholeTask = (fmap . fmap) wholeTaskDistance lsTask
        lsSpeedSubset = (fmap . fmap) speedSubsetDistance lsTask

        -- Velocities (vs).
        ssVs :: [Maybe (BestTime (Quantity Double [u| h |]), [(Pilot, TrackSpeed)])] =
                times ssTime <$> yss

        gsVs :: [Maybe (BestTime (Quantity Double [u| h |]), [(Pilot, TrackSpeed)])] =
                times gsTime <$> yss

        dsAlt :: [[(Pilot, QAlt Double [u| m |])]] = landAltitudes <$> yss

        -- Times (ts).
        ssBestTime = (fmap . fmap) fst ssVs
        gsBestTime = (fmap . fmap) fst gsVs

