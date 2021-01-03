{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Flight.Track.Mask.Effort
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks masked with task control zones.
-}
module Flight.Track.Mask.Effort
    ( TaskMaskingEffort(..)
    , CompMaskingEffort(..)
    , mkCompMaskEffort, unMkCompMaskEffort
    ) where

import Data.UnitsOfMeasure (u)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Distance (QTaskDistance)
import "flight-gap-allot" Flight.Score (Pilot(..))
import Flight.Field (FieldOrdering(..))
import Flight.Units ()
import Flight.Track.Distance (TrackDistance(..), Effort)
import Flight.Track.Mask.Cmp (cmp)

data TaskMaskingEffort =
    TaskMaskingEffort
        { bestEffort :: Maybe (QTaskDistance Double [u| m |])
        -- ^ The best distance made.
        , land :: [(Pilot, TrackDistance Effort)]
        -- ^ The distance of the landing spot for each pilot landing out.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | For each task, the masking for effort for that task.
data CompMaskingEffort =
    CompMaskingEffort
        { bestEffort :: [Maybe (QTaskDistance Double [u| m |])]
        -- ^ For each task, the best distance made.
        , land :: [[(Pilot, TrackDistance Effort)]]
        -- ^ For each task, the distance of the landing spot for each pilot
        -- landing out.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

mkCompMaskEffort :: [TaskMaskingEffort] -> CompMaskingEffort
mkCompMaskEffort ts =
    uncurry CompMaskingEffort $ unzip
    [ (b, l)
    | TaskMaskingEffort{bestEffort = b, land = l} <- ts
    ]

unMkCompMaskEffort :: CompMaskingEffort -> [TaskMaskingEffort]
unMkCompMaskEffort CompMaskingEffort{bestEffort = bs, land = ls} =
    zipWith TaskMaskingEffort bs ls

instance FieldOrdering TaskMaskingEffort where fieldOrder _ = cmp
instance FieldOrdering CompMaskingEffort where fieldOrder _ = cmp
