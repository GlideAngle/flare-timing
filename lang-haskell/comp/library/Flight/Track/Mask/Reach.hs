{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Flight.Track.Mask.Reach
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks masked with task control zones.
-}
module Flight.Track.Mask.Reach
    ( TaskMaskingReach(..)
    , CompMaskingReach(..)
    , mkCompMaskReach, unMkCompMaskReach
    ) where

import GHC.Generics (Generic)
import Data.String (IsString())
import Data.List (unzip4)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Units ()
import Flight.Field (FieldOrdering(..))
import "flight-gap-allot" Flight.Score (Pilot(..))
import "flight-gap-valid" Flight.Score (ReachStats)
import Flight.Track.Distance (TrackDistance(..), TrackReach(..), Nigh)
import qualified Flight.Track.Mask.Cmp as Cmp (cmp)
import Flight.Track.Curry (uncurry4)

data TaskMaskingReach =
    TaskMaskingReach
        { bolster :: Maybe ReachStats
        -- ^ The bolstered reach, reach clamped below to minimum distance.
        , reach :: Maybe ReachStats
        -- ^ The reach as flown, possibly less than minimum distance.
        , reachRank :: [(Pilot, TrackReach)]
        -- ^ For each task, the rank order of reach and linear distance fraction.
        , nigh :: [(Pilot, TrackDistance Nigh)]
        -- ^ The best distance of each pilot landing out.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | For each task, the masking for reach for that task.
data CompMaskingReach =
    CompMaskingReach
        { bolster :: [Maybe ReachStats]
        -- ^ The bolstered reach, reach clamped below to minimum distance.
        , reach :: [Maybe ReachStats]
        -- ^ The reach as flown, possibly less than minimum distance.
        , reachRank :: [[(Pilot, TrackReach)]]
        -- ^ For each task, the rank order of reach and linear distance fraction.
        , nigh :: [[(Pilot, TrackDistance Nigh)]]
        -- ^ For each task, the best distance of each pilot landing out.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

mkCompMaskReach :: [TaskMaskingReach] -> CompMaskingReach
mkCompMaskReach ts =
    uncurry4 CompMaskingReach $ unzip4
    [ (b, r, k, n)
    | TaskMaskingReach{bolster = b, reach = r, reachRank = k, nigh = n} <- ts
    ]

unMkCompMaskReach :: CompMaskingReach -> [TaskMaskingReach]
unMkCompMaskReach CompMaskingReach{bolster = bs, reach = rs, reachRank = ks, nigh = ns} =
    [TaskMaskingReach b r k n | b <- bs | r <- rs | k <- ks | n <- ns]

instance FieldOrdering TaskMaskingReach where fieldOrder _ = cmp
instance FieldOrdering CompMaskingReach where fieldOrder _ = cmp

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
