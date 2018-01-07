{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Flight.Score
    ( Lookahead
    , Chunks
    , SumOfDifficulty
    , ChunkRelativeDifficulty
    , ChunkDifficultyFraction
    , ChunkLandings
    )

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
        , sumOfDifficulty :: [Maybe SumOfDifficulty]
        -- ^ The difficulty of each chunk is relative to the sum of
        -- difficulties.
        , relativeDifficulty :: [Maybe [ChunkRelativeDifficulty]]
        -- ^ The relative difficulty of each chunk.
        , fractionalDifficulty :: [Maybe [ChunkDifficultyFraction]]
        -- ^ The fractional difficulty, being the sum of relative difficulties
        -- up to the chunk of landing.
        , chunkLandings :: [[ChunkLandings]]
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

        ("sumOfDifficulty", "minDistance") -> GT
        ("sumOfDifficulty", "bestDistance") -> GT
        ("sumOfDifficulty", "landout") -> GT
        ("sumOfDifficulty", "lookahead") -> GT
        ("sumOfDifficulty", _) -> LT

        ("relativeDifficulty", "minDistance") -> GT
        ("relativeDifficulty", "bestDistance") -> GT
        ("relativeDifficulty", "landout") -> GT
        ("relativeDifficulty", "lookahead") -> GT
        ("relativeDifficulty", "sumOfDifficulty") -> GT
        ("relativeDifficulty", _) -> LT

        ("fractionalDifficulty", "minDistance") -> GT
        ("fractionalDifficulty", "bestDistance") -> GT
        ("fractionalDifficulty", "landout") -> GT
        ("fractionalDifficulty", "lookahead") -> GT
        ("fractionalDifficulty", "sumOfDifficulty") -> GT
        ("fractionalDifficulty", "relativeDifficulty") -> GT
        ("fractionalDifficulty", _) -> LT

        ("chunkLandings", "minDistance") -> GT
        ("chunkLandings", "bestDistance") -> GT
        ("chunkLandings", "landout") -> GT
        ("chunkLandings", "lookahead") -> GT
        ("chunkLandings", "sumOfDifficulty") -> GT
        ("chunkLandings", _) -> LT

        ("chunks", _) -> GT

        _ -> compare a b
