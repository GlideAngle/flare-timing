{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Flight.Track.Mask.Speed
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks masked with task control zones.
-}
module Flight.Track.Mask.Speed
    ( TaskMaskingSpeed(..)
    , CompMaskingSpeed(..)
    , mkCompMaskSpeed, unMkCompMaskSpeed
    ) where

import Data.List (unzip7)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Distance (QTaskDistance)
import Flight.LatLng (QAlt)
import "flight-gap-allot" Flight.Score (Pilot(..), BestTime(..))
import Flight.Field (FieldOrdering(..))
import Flight.Units ()
import Flight.Track.Speed (TrackSpeed(..))
import Flight.Track.Mask.Cmp (cmp)
import Flight.Track.Curry (uncurry7)

data TaskMaskingSpeed =
    TaskMaskingSpeed
        { ssBestTime :: Maybe (BestTime (Quantity Double [u| h |]))
        -- ^ The best time ignoring start gates.
        , gsBestTime :: Maybe (BestTime (Quantity Double [u| h |]))
        -- ^ The best time from the start gate taken.
        , taskDistance :: Maybe (QTaskDistance Double [u| m |])
        -- ^ The task distance.
        , taskSpeedDistance :: Maybe (QTaskDistance Double [u| m |])
        -- ^ The speed section subset of the task distance.
        , ssSpeed :: [(Pilot, TrackSpeed)]
        -- ^ For each pilot making goal, their time for the
        -- speed section and speed fraction, ignoring any start gates.
        , gsSpeed :: [(Pilot, TrackSpeed)]
        -- ^ For each pilot making goal, their time for the
        -- speed section and speed fraction, taking into account start gates.
        , altStopped :: [(Pilot, QAlt Double [u| m |])]
        -- ^ The altitude of each pilot at the score back time.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | For each task, the masking for speed for that task.
data CompMaskingSpeed =
    CompMaskingSpeed
        { ssBestTime :: [Maybe (BestTime (Quantity Double [u| h |]))]
        -- ^ For each task, the best time ignoring start gates.
        , gsBestTime :: [Maybe (BestTime (Quantity Double [u| h |]))]
        -- ^ For each task, the best time from the start gate taken.
        , taskDistance :: [Maybe (QTaskDistance Double [u| m |])]
        -- ^ For each task, the task distance.
        , taskSpeedDistance :: [Maybe (QTaskDistance Double [u| m |])]
        -- ^ For each task, the speed section subset of the task distance.
        , ssSpeed :: [[(Pilot, TrackSpeed)]]
        -- ^ For each task, for each pilot making goal, their time for the
        -- speed section and speed fraction, ignoring any start gates.
        , gsSpeed :: [[(Pilot, TrackSpeed)]]
        -- ^ For each task, for each pilot making goal, their time for the
        -- speed section and speed fraction, taking into account start gates.
        , altStopped :: [[(Pilot, QAlt Double [u| m |])]]
        -- ^ For each task, the altitude of each pilot at the score back time.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

mkCompMaskSpeed :: [TaskMaskingSpeed] -> CompMaskingSpeed
mkCompMaskSpeed ts =
    uncurry7 CompMaskingSpeed $ unzip7
    [ (a, b, c, d, e, f, g)
    | TaskMaskingSpeed
        { ssBestTime = a
        , gsBestTime = b
        , taskDistance = c
        , taskSpeedDistance = d
        , ssSpeed = e
        , gsSpeed = f
        , altStopped = g
        } <- ts
    ]

unMkCompMaskSpeed :: CompMaskingSpeed -> [TaskMaskingSpeed]
unMkCompMaskSpeed
    CompMaskingSpeed
        { ssBestTime = as
        , gsBestTime = bs
        , taskDistance = cs
        , taskSpeedDistance = ds
        , ssSpeed = es
        , gsSpeed = fs
        , altStopped = gs
        } =
    [ TaskMaskingSpeed a b c d e f g
    | a <- as
    | b <- bs
    | c <- cs
    | d <- ds
    | e <- es
    | f <- fs
    | g <- gs
    ]

instance FieldOrdering TaskMaskingSpeed where fieldOrder _ = cmp
instance FieldOrdering CompMaskingSpeed where fieldOrder _ = cmp
