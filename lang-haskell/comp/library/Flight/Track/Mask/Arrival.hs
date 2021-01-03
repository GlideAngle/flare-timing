{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Flight.Track.Mask.Arrrival
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks masked with task control zones.
-}
module Flight.Track.Mask.Arrival
    ( TaskMaskingArrival(..)
    , CompMaskingArrival(..)
    , mkCompMaskArrival, unMkCompMaskArrival
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import "flight-gap-allot" Flight.Score (Pilot(..), PilotsAtEss(..))
import Flight.Field (FieldOrdering(..))
import Flight.Units ()
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Mask.Cmp (cmp)

data TaskMaskingArrival =
    TaskMaskingArrival
        { pilotsAtEss :: PilotsAtEss
        -- ^ The number of pilots at ESS.
        , arrivalRank :: [(Pilot, TrackArrival)]
        -- ^ The rank order of arrival at ESS and arrival fraction.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)


-- | For each task, the masking for arrival for that task.
data CompMaskingArrival =
    CompMaskingArrival
        { pilotsAtEss :: [PilotsAtEss]
        -- ^ For each task, the number of pilots at ESS.
        , arrivalRank :: [[(Pilot, TrackArrival)]]
        -- ^ For each task, the rank order of arrival at ESS and arrival fraction.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

mkCompMaskArrival :: [TaskMaskingArrival] -> CompMaskingArrival
mkCompMaskArrival ts =
    uncurry CompMaskingArrival $ unzip
    [ (p, a)
    | TaskMaskingArrival{pilotsAtEss = p, arrivalRank = a} <- ts
    ]

unMkCompMaskArrival :: CompMaskingArrival -> [TaskMaskingArrival]
unMkCompMaskArrival CompMaskingArrival{pilotsAtEss = ps, arrivalRank = as} =
    zipWith TaskMaskingArrival ps as

instance FieldOrdering TaskMaskingArrival where fieldOrder _ = cmp
instance FieldOrdering CompMaskingArrival where fieldOrder _ = cmp
