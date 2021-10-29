{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
module MaskEffort (maskEffort, landDistances) where

import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Data.UnitsOfMeasure (u)

import Flight.Distance (QTaskDistance)
import Flight.Track.Mask (CompMaskingEffort(..))
import Flight.Comp (Pilot)
import Flight.Track.Distance (TrackDistance(..), Effort)
import Stats (FlightStats(..))

landDistances :: [(Pilot, FlightStats k)] -> [(Pilot, TrackDistance Effort)]
landDistances xs =
    sortOn (togo . snd)
    . catMaybes
    $ fmap (\(p, FlightStats{..}) -> (p,) <$> statEffort) xs

maskEffort
    :: [Maybe (QTaskDistance Double [u| m |])]
    -> [[(Pilot, TrackDistance Effort)]]
    -> CompMaskingEffort
maskEffort dsBest dsEffort =
    CompMaskingEffort
        { bestEffort = dsBest
        , land = dsEffort
        }
