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
    ( Landing(..)
    , TaskLanding(..)
    , TrackEffort(..)
    , compLanding
    , taskLanding
    , effortRank
    ) where

import Control.Lens ((^?), element)
import Control.Monad (join)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (sortOn)
import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Field (FieldOrdering(..))
import Flight.Distance (QTaskDistance, TaskDistance(..))
import Flight.Score
    ( Lookahead
    , Chunking(..)
    , ChunkDifficulty(..)
    , MinimumDistance(..)
    , FlownMax(..)
    , PilotDistance(..)
    , DifficultyFraction(..)
    , Pilot(..)
    )
import Flight.Comp (IxTask(..))

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

effortRank :: Landing -> [[(Pilot, TrackEffort)]]
effortRank Landing{difficulty} =
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

taskLanding :: IxTask -> Landing -> Maybe TaskLanding
taskLanding (IxTask iTask) Landing{..} = do
    let i = fromIntegral iTask - 1
    bd <- bestDistance ^? element i
    lo <- landout ^? element i
    la <- lookahead ^? element i
    cg <- chunking ^? element i
    df <- difficulty ^? element i
    return
        TaskLanding
            { minDistance = minDistance
            , bestDistance = bd
            , landout = lo
            , lookahead = la
            , chunking = cg
            , difficulty = df
            }

-- | The landing for a single task.
data TaskLanding =
    TaskLanding
        { minDistance :: MinimumDistance (Quantity Double [u| km |])
        -- ^ The mimimum distance, set once for the comp. All pilots landing
        -- before this distance get this distance. The 100m segments start from
        -- here.
        , bestDistance :: Maybe (FlownMax (Quantity Double [u| km |]))
        -- ^ For each task, the best distance flown.
        , landout :: Int
        -- ^ For each task, the number of pilots landing out.
        , lookahead :: Maybe Lookahead
        -- ^ For each task, how many 100m chunks to look ahead for land outs.
        , chunking :: Maybe Chunking
        -- ^ For each task, the chunking.
        , difficulty :: Maybe [ChunkDifficulty]
        -- ^ The difficulty of each chunk.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

compLanding :: MinimumDistance (Quantity Double [u| km |]) -> [Maybe TaskLanding] -> Landing
compLanding free xs =
    Landing
        { minDistance =
            fromMaybe free . listToMaybe $
            [ fromMaybe free $ (\TaskLanding{minDistance = md} -> md) <$> x | x <- xs ]
        , bestDistance =
            [ join $ (\TaskLanding{bestDistance = bd} -> bd) <$> x | x <- xs ]
        , landout =
            [ maybe 0 (\TaskLanding{landout = lo} -> lo) x | x <- xs ]
        , lookahead =
            [ join $ (\TaskLanding{lookahead = ahead} -> ahead) <$> x| x <- xs ]
        , chunking =
            [ join $ (\TaskLanding{chunking = cg} -> cg) <$> x| x <- xs ]
        , difficulty =
            [ join $ (\TaskLanding{difficulty = dy} -> dy) <$> x| x <- xs ]
        }

-- | For each task, the landing for that task.
data Landing =
    Landing
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

instance FieldOrdering Landing where
    fieldOrder _ = cmp

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
