{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Flight.Gap.Distance.Difficulty
    ( SumOfDifficulty(..)
    , Difficulty(..)
    , difficulty
    ) where

import Data.Ratio ((%))
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import GHC.Generics (Generic)

import Flight.Units ()
import Flight.Gap.Distance.Linear (BestDistance(..), PilotDistance(..))
import Flight.Gap.Distance.Min (MinimumDistance(..))
import Flight.Gap.Distance.Relative (RelativeDifficulty(..))
import Flight.Gap.Distance.Fraction (DifficultyFraction(..))
import Flight.Gap.Distance.Chunk
    ( IxChunk(..)
    , Lookahead(..)
    , ChunkLandings(..)
    , ChunkRelativeDifficulty(..)
    , ChunkDifficultyFraction(..)
    , lookahead
    , chunkLandouts
    , sumLandouts
    )

-- | The sum of all chunk difficulties.
newtype SumOfDifficulty = SumOfDifficulty Integer
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Difficulty =
    Difficulty
        { sumOf :: SumOfDifficulty
        -- ^ The sum of the downward counts.
        , downward :: [ChunkLandings]
        -- ^ The number on their way down for a landing between this chunk and
        -- the lookahead offset.
        , relative :: [ChunkRelativeDifficulty]
        -- ^ The relative difficulty of each chunk.
        , fractional :: [ChunkDifficultyFraction]
        -- ^ The fractional difficulty of each chunk.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | For a list of distances flown by pilots, works out the distance difficulty
-- fraction for each pilot.
difficulty
    :: MinimumDistance (Quantity Double [u| km |])
    -> BestDistance (Quantity Double [u| km |])
    -> [PilotDistance (Quantity Double [u| km |])]
    -> Difficulty
difficulty md best xs =
    Difficulty
        { sumOf = SumOfDifficulty sumOfDiff
        , downward = uncurry ChunkLandings <$> downs
        , relative =
            (uncurry ChunkRelativeDifficulty . (fmap RelativeDifficulty))
            <$> rels
        , fractional =
            (uncurry ChunkDifficultyFraction . (fmap DifficultyFraction))
            <$> fracs
        }
    where
        ys :: [IxChunk]
        ys = chunkLandouts md xs

        ns :: [(IxChunk, Int)]
        ns = sumLandouts ys

        nMap :: Map.Map IxChunk Int
        nMap = Map.fromList ns

        n = lookahead best xs
        sumOfDowns = sumLandoutsAhead n nMap

        lookaheadMap :: Map.Map IxChunk Integer
        lookaheadMap = toInteger <$> sumOfDowns 

        sumOfDiff :: Integer
        sumOfDiff = toInteger $ sum $ Map.elems lookaheadMap

        relativeDiffMap :: Map.Map IxChunk Rational
        relativeDiffMap = (\d -> d % (2 * sumOfDiff)) <$> lookaheadMap

        downs = sortOn fst $ Map.toList sumOfDowns
        rels = sortOn fst $ Map.toList relativeDiffMap
        fracs = scanl1 (\(_, b) (c, d) -> (c, b + d)) rels

-- | Sums the number of landouts in the next so many chunks to lookahead.
sumLandoutsAhead :: Lookahead -> Map.Map IxChunk Int -> Map.Map IxChunk Int
sumLandoutsAhead (Lookahead n) nMap =
    Map.mapWithKey f nMap
    where
        f :: IxChunk -> Int -> Int
        f (IxChunk ic) _ =
            sum $ Map.elems filtered
            where
                kMin = ic 
                kMax = ic + n

                filtered =
                    Map.filterWithKey
                        (\(IxChunk k) _ -> kMin <= k && k <= kMax)
                        nMap
