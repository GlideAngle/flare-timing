{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Flight.Track.Mask
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks masked with task control zones.
-}
module Flight.Track.Land
    ( TaskLanding(..)
    , CompLanding(..)
    , TrackEffort(..)
    , effortRank
    , mkCompLandOut, unMkCompLandOut, taskLanding
    ) where

import Lens.Micro ((^?), ix)
import Data.List (sortOn, unzip5)
import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Field (FieldOrdering(..))
import Flight.Distance (QTaskDistance, TaskDistance(..))
import "flight-gap-allot" Flight.Score
    ( Pilot(..)
    , PilotDistance(..)
    , MinimumDistance(..)
    , FlownMax(..)
    )
import "flight-gap-effort" Flight.Score
    ( Lookahead
    , Chunking(..)
    , ChunkDifficulty(..)
    , DifficultyFraction(..)
    )
import Flight.Comp (IxTask(..))
import Flight.Track.Curry (uncurry5)

data TrackEffort =
    TrackEffort
        { effort :: QTaskDistance Double [u| m |]
        , frac :: DifficultyFraction
        -- ^ This is a fraction of the possible effort points, not of the
        -- possible distance points.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

chunkEffort :: ChunkDifficulty -> [(Pilot, TrackEffort)]
chunkEffort ChunkDifficulty{downs, downers, frac = DifficultyFraction df} =
    [
        ( pilot
        , TrackEffort
            { effort = TaskDistance $ convert d
            -- NOTE: Make maximum track effort unit, 1. Difficulty points are
            -- half of the distance points so we need to double the fraction.
            , frac = DifficultyFraction $ 2 * df
            }
        )
    | PilotDistance d <- downs
    | pilot <- downers
    ]

effortRank :: CompLanding -> [[(Pilot, TrackEffort)]]
effortRank CompLanding{difficulty} =
    [ maybe
        []
        ( sortOn
            ( (\TrackEffort{frac = DifficultyFraction x} -> negate x)
            . snd
            )
        . concat
        . fmap chunkEffort
        )
        d
    | d <- difficulty
    ]

-- | The landing for a single task.
data TaskLanding =
    TaskLanding
        { minDistance :: MinimumDistance (Quantity Double [u| km |])
        -- ^ The mimimum distance, set once for the comp. All pilots landing
        -- before this distance get this distance. The 100m segments start from
        -- here.
        , bestDistance :: Maybe (FlownMax (Quantity Double [u| km |]))
        -- ^ The best distance flown.
        , landout :: Int
        -- ^ The number of pilots landing out.
        , lookahead :: Maybe Lookahead
        -- ^ How many 100m chunks to look ahead for land outs.
        , chunking :: Maybe Chunking
        -- ^ The chunking.
        , difficulty :: Maybe [ChunkDifficulty]
        -- ^ The difficulty of each chunk.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | For each task, the landing for that task.
data CompLanding =
    CompLanding
        { minDistance :: MinimumDistance (Quantity Double [u| km |])
        -- ^ The mimimum distance, set once for the comp. All pilots landing
        -- before this distance get this distance. The 100m segments start from
        -- here.
        , bestDistance :: [Maybe (FlownMax (Quantity Double [u| km |]))]
        -- ^ For each task, the best distance flown.
        , landout :: [Int]
        -- ^ For each task, the number of pilots landing out.
        , lookahead :: [Maybe Lookahead]
        -- ^ For each task, how many 100m chunks to look ahead for land outs.
        , chunking :: [Maybe Chunking]
        -- ^ For each task, the chunking.
        , difficulty :: [Maybe [ChunkDifficulty]]
        -- ^ The difficulty of each chunk.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

mkCompLandOut :: [TaskLanding] -> CompLanding
mkCompLandOut ts =
    (\(a' : _, xs) -> uncurry5 (CompLanding a') $ unzip5 xs) $ unzip
    [ (a, (b, c, d, e, f))
    | TaskLanding
        { minDistance = a
        , bestDistance = b
        , landout = c
        , lookahead = d
        , chunking = e
        , difficulty = f
        } <- ts
    ]

unMkCompLandOut :: CompLanding -> [TaskLanding]
unMkCompLandOut
    CompLanding
        { minDistance = a
        , bestDistance = bs
        , landout = cs
        , lookahead = ds
        , chunking = es
        , difficulty = fs
        } =
    [ TaskLanding a b c d e f
    | b <- bs
    | c <- cs
    | d <- ds
    | e <- es
    | f <- fs
    ]

taskLanding :: IxTask -> CompLanding -> Maybe TaskLanding
taskLanding (IxTask iTask) CompLanding{..} = do
    let i = fromIntegral iTask - 1
    bd <- bestDistance ^? ix i
    lo <- landout ^? ix i
    la <- lookahead ^? ix i
    cg <- chunking ^? ix i
    df <- difficulty ^? ix i
    return
        TaskLanding
            { minDistance = minDistance
            , bestDistance = bd
            , landout = lo
            , lookahead = la
            , chunking = cg
            , difficulty = df
            }

instance FieldOrdering TaskLanding where fieldOrder _ = cmp
instance FieldOrdering CompLanding where fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        ("chunk", _) -> LT

        ("startChunk", "chunk") -> GT
        ("startChunk", _) -> LT

        ("endChunk", "chunk") -> GT
        ("endChunk", "startChunk") -> GT
        ("endChunk", _) -> LT

        ("endAhead", "chunk") -> GT
        ("endAhead", "startChunk") -> GT
        ("endAhead", "endChunk") -> GT
        ("endAhead", _) -> LT

        ("down", "chunk") -> GT
        ("down", "startChunk") -> GT
        ("down", "endChunk") -> GT
        ("down", "endAhead") -> GT
        ("down", _) -> LT

        ("downs", "chunk") -> GT
        ("downs", "startChunk") -> GT
        ("downs", "endChunk") -> GT
        ("downs", "endAhead") -> GT
        ("downs", "down") -> GT
        ("downs", _) -> LT

        ("downers", "chunk") -> GT
        ("downers", "startChunk") -> GT
        ("downers", "endChunk") -> GT
        ("downers", "endAhead") -> GT
        ("downers", "down") -> GT
        ("downers", "downs") -> GT
        ("downers", _) -> LT

        ("downward", "chunk") -> GT
        ("downward", "startChunk") -> GT
        ("downward", "endChunk") -> GT
        ("downward", "endAhead") -> GT
        ("downward", "down") -> GT
        ("downward", "downs") -> GT
        ("downward", "downers") -> GT
        ("downward", _) -> LT

        ("rel", "chunk") -> GT
        ("rel", "startChunk") -> GT
        ("rel", "endChunk") -> GT
        ("rel", "endAhead") -> GT
        ("rel", "down") -> GT
        ("rel", "downs") -> GT
        ("rel", "downers") -> GT
        ("rel", "downward") -> GT
        ("rel", _) -> LT

        ("frac", _) -> GT

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

        ("difficulty", _) -> GT

        _ -> compare a b
