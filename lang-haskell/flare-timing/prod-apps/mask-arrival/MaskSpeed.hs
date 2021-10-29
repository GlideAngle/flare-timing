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
import Flight.Track.Mask (CompMaskingSpeed(..))
import Flight.Track.Speed (TrackSpeed(..))
import qualified "flight-gap-allot" Flight.Score as Gap (bestTime')
import "flight-gap-allot" Flight.Score
    (BestTime(..), PilotTime(..), PowerExponent(..), speedFraction)
import Stats (TimeStats(..), FlightStats(..))

landAltitudes :: [(Pilot, FlightStats k)] -> [(Pilot, QAlt Double [u| m |])]
landAltitudes xs =
    catMaybes
    $ fmap (\(p, FlightStats{..}) -> (p,) <$> statAlt) xs

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

maskSpeed
    :: PowerExponent
    -> [Maybe TaskRouteDistance]
    -> [[(Pilot, FlightStats k)]]
    -> ([Maybe (BestTime (Quantity Double [u| h |]))], CompMaskingSpeed)
maskSpeed pe lsTask yss =
    (gsBestTime,) $
    CompMaskingSpeed
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
                times pe ssTime <$> yss

        gsVs :: [Maybe (BestTime (Quantity Double [u| h |]), [(Pilot, TrackSpeed)])] =
                times pe gsTime <$> yss

        dsAlt :: [[(Pilot, QAlt Double [u| m |])]] = landAltitudes <$> yss

        -- Times (ts).
        ssBestTime = (fmap . fmap) fst ssVs
        gsBestTime = (fmap . fmap) fst gsVs

