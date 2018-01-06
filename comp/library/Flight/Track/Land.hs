{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Flight.Track.Mask
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks masked with task control zones.
-}
module Flight.Track.Land
    ( Landing(..)
    ) where

import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Field (FieldOrdering(..))
import Flight.Score (Lookahead, Chunks, ChunkedDistance)

-- | For each task, the masking for that task.
data Landing =
    Landing 
        { minDistance :: Double
        -- ^ The mimimum distance, set once for the comp. All pilots landing
        -- before this distance get this distance. The 100m segments start from
        -- here.
        , bestDistance :: [Maybe Double]
        -- ^ For each task, the best distance flown.
        , landout :: [Int]
        -- ^ For each task, the number of pilots landing out.
        , lookahead :: [Maybe Lookahead]
        -- ^ For each task, how many 100m chunks to look ahead for land outs.
        , chunkLandings :: [[(ChunkedDistance, Int)]]
        -- ^ For each task, the number of landouts in each chunk, chunks with
        -- no landouts ommitted.
        , chunks :: [Chunks]
        -- ^ For each task, the task distance to the end of each 100m chunk.
        }
        deriving (Eq, Ord, Show, Generic)

instance ToJSON Landing
instance FromJSON Landing

instance FieldOrdering Landing where
    fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        ("minDistance", _) -> LT

        ("bestDistance", "minDistance") -> GT
        ("bestDistance", _) -> LT

        ("landout", "minDistance") -> GT
        ("landout", "bestDistance") -> GT
        ("landout", _) -> LT

        ("lookahead", "minDistance") -> GT
        ("lookahead", "bestDistance") -> GT
        ("lookahead", "landout") -> GT
        ("lookahead", _) -> LT

        ("chunkLandings", "minDistance") -> GT
        ("chunkLandings", "bestDistance") -> GT
        ("chunkLandings", "landout") -> GT
        ("chunkLandings", "lookahead") -> GT
        ("chunkLandings", _) -> LT

        ("chunks", _) -> GT

        _ -> compare a b
