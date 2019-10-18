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
import Data.String (IsString())
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Score (Pilot(..))
import Flight.Field (FieldOrdering(..))
import Flight.Units ()
import Flight.Track.Distance (TrackDistance(..), TrackReach(..), Nigh)
import qualified Flight.Track.Mask.Cmp as Cmp (cmp)
import Flight.Score (ReachStats)

-- | For each task, the masking for reach for that task.
data MaskingReach =
    MaskingReach
        { bolster :: [ReachStats]
        -- ^ The bolstered reach, reach clamped below to minimum distance.
        , reach :: [ReachStats]
        -- ^ The reach as flown, possibly less than minimum distance.
        -- ^ For each task, the best distance made.
        , reachRank :: [[(Pilot, TrackReach)]]
        -- ^ For each task, the rank order of reach and linear distance fraction.
        , nigh :: [[(Pilot, TrackDistance Nigh)]]
        -- ^ For each task, the best distance of each pilot landing out.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance FieldOrdering MaskingReach where fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        -- TODO: first start time & last goal time & launched
        ("bolster", _) -> LT

        ("reach", "bolster") -> GT
        ("reach", "frac") -> GT
        ("reach", _) -> LT

        ("reachRank", "bolster") -> GT
        ("reachRank", "reach") -> GT
        ("reachRank", _) -> LT

        _ -> Cmp.cmp a b
