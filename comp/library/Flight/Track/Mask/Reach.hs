{-|
Module      : Flight.Track.Mask.Reach
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks masked with task control zones.
-}
module Flight.Track.Mask.Reach (MaskingReach(..)) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (QTaskDistance)
import Flight.Score (Pilot(..))
import Flight.Field (FieldOrdering(..))
import Flight.Units ()
import Flight.Track.Distance (TrackDistance(..), TrackReach(..), Nigh)
import Flight.Track.Mask.Cmp (cmp)
import Flight.Score

-- | For each task, the masking for reach for that task.
data MaskingReach =
    MaskingReach
        { flownMax :: [Maybe (FlownMax (Quantity Double [u| m |]))]
        -- ^ For each task, the best distance made.
        , flownMean :: [QTaskDistance Double [u| m |]]
        -- ^ For each task, the mean of the flown distance, reach clamped below
        -- to minimum distance.
        , flownStdDev :: [QTaskDistance Double [u| m |]]
        -- ^ For each task, the standard deviation of flown distance, reach
        -- clamped below to minimum distance.
        , reachMean :: [QTaskDistance Double [u| m |]]
        -- ^ For each task, the mean of reach.
        , reachStdDev :: [QTaskDistance Double [u| m |]]
        -- ^ For each task, the standard deviation of reach.
        , reachRank :: [[(Pilot, TrackReach)]]
        -- ^ For each task, the rank order of reach and linear distance fraction.
        , nigh :: [[(Pilot, TrackDistance Nigh)]]
        -- ^ For each task, the best distance of each pilot landing out.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance FieldOrdering MaskingReach where fieldOrder _ = cmp
