{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
module MaskEffort (maskEffort) where

import Data.UnitsOfMeasure (u)

import Flight.Distance (QTaskDistance)
import Flight.Track.Mask (MaskingEffort(..))
import Flight.Comp (Pilot)
import Flight.Track.Distance (TrackDistance(..), Land)

maskEffort
    :: [Maybe (QTaskDistance Double [u| m |])]
    -> [[(Pilot, TrackDistance Land)]]
    -> MaskingEffort
maskEffort dsBest dsLand =
    MaskingEffort
        { bestEffort = dsBest
        , land = dsLand
        }
