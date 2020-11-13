{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
module MaskEffort (maskEffort, landDistances) where

import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Data.UnitsOfMeasure (u)

import Flight.Distance (QTaskDistance)
import Flight.Track.Mask (MaskingEffort(..))
import Flight.Comp (Pilot)
import Flight.Track.Distance (TrackDistance(..), Land)
import Stats (FlightStats(..))

landDistances :: [(Pilot, FlightStats k)] -> [(Pilot, TrackDistance Land)]
landDistances xs =
    sortOn (togo . snd)
    . catMaybes
    $ fmap (\(p, FlightStats{..}) -> (p,) <$> statLand) xs

maskEffort
    :: [Maybe (QTaskDistance Double [u| m |])]
    -> [[(Pilot, TrackDistance Land)]]
    -> MaskingEffort
maskEffort dsBest dsLand =
    MaskingEffort
        { bestEffort = dsBest
        , land = dsLand
        }
