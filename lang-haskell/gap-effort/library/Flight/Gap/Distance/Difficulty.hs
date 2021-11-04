{-# LANGUAGE DuplicateRecordFields #-}

module Flight.Gap.Distance.Difficulty
    ( SumOfDifficulty(..)
    , Chunking(..)
    , Difficulty(..)
    , gradeDifficulty
    ) where

import Data.Ratio ((%))
import Data.Maybe (catMaybes)
import Data.Ord(Down(Down))
import Data.List (sortOn, nub)
import qualified Data.Map.Strict as Map
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure ((+:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import GHC.Generics (Generic)

import Flight.Units ()
import "flight-gap-allot" Flight.Score
    ( PilotDistance(..), FlownMax(..), Pilot, DifficultyFraction(..))
import Flight.Gap.Distance.Relative (RelativeDifficulty(..))
import Flight.Gap.Distance.Chunk
    ( IxChunk(..)
    , Lookahead(..)
    , Chunk(..)
    , ChunkLandings(..)
    , ChunkRelativeDifficulty(..)
    , ChunkDifficultyFraction(..)
    , toChunk
    , toIxChunk
    , lookahead
    , chunkLandouts
    , sumLandouts
    , collectDowns
    )

-- | The sum of all chunk difficulties.
newtype SumOfDifficulty = SumOfDifficulty Integer
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | The range of chunking used in working out difficulty.  For each chunk in
-- the range, add together the number of pilots down in that chunk to the
-- number of pilots downward bound within the lookahead number of chunks to get
-- the difficulty of a single chunk. The ratio of this to the sum of all
-- difficulty is the relative difficulty of a chunk. To get to the landing
-- chunk, the pilot had to overcome the difficulty of each chunk flown over.
-- The difficulty fraction then is the sum of difficulties up until the chunk
-- of landing.
--
-- Note that in the GAP docs and here, this fraction is not normalized. It is
-- expressed as a fraction of the maximum distance points and so is in the
-- range zero to a half as only half of the distance points are available for
-- difficulty.
data Chunking =
    Chunking
        { sumOf :: SumOfDifficulty
        -- ^ The sum of the downward counts.
        , startChunk :: (IxChunk, Chunk (Quantity Double [u| km |]))
        -- ^ The task distance to the start of this chunk.
        , endChunk :: (IxChunk, Chunk (Quantity Double [u| km |]))
        -- ^ The task distance to the end of this chunk.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | A sparse record of the difficulty for each chunk landed in.
data Difficulty =
    Difficulty
        { startChunk :: [(IxChunk, Chunk (Quantity Double [u| km |]))]
        -- ^ The task distance to the start of this chunk.
        , endChunk :: [(IxChunk, Chunk (Quantity Double [u| km |]))]
        -- ^ The task distance to the end of this chunk.
        , endAhead :: [(IxChunk, Chunk (Quantity Double [u| km |]))]
        -- ^ The task distance to the end of lookahead chunk.
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
-- fraction for each pilot. A consensus on difficulty is attained by counting
-- those who landout in sections of the course. A section is drawn from the
-- chunk of landing and then so many chunks further along the course. How far
-- to look ahead depends on the task and the number of landouts.
gradeDifficulty
    :: FlownMax (Quantity Double [u| km |])
    -> [Pilot]
    -> [PilotDistance (Quantity Double [u| km |])]
    -> (Chunking, Difficulty)
gradeDifficulty best@(FlownMax (MkQuantity bd)) pilots landings =
    ( Chunking
        { sumOf = SumOfDifficulty sumOfDiff
        , startChunk = (ix0, toChunk ix0)
        , endChunk = (ixN, toChunk ixN)
        }
    , Difficulty
        { startChunk = zip ys nubStarts
        , endChunk = zip ys nubEnds
        , endAhead = zip ys nubEndsAhead
        , downward = collectDowns pilots xs downList
        , relative =
            catMaybes $
            (\y -> do
                rel <- Map.lookup y relMap
                let f = uncurry ChunkRelativeDifficulty . fmap (RelativeDifficulty . realToFrac)
                return $ f (y, rel))
            <$> ys
        , fractional =
            catMaybes $
            (\y -> do
                frac <- Map.lookup y fracMap
                let f = uncurry ChunkDifficultyFraction . fmap (DifficultyFraction . realToFrac)
                return $ f (y, frac))
            <$> ys
        }
    )
    where
        -- When pilots fly away from goal looking for lift but land out they
        -- can end up with a negative distance along the course. We'll zero
        -- these landings before starting on course difficulty.
        xs = (\(PilotDistance d) -> PilotDistance $ max d [u| 0 km |]) <$> landings

        ahead@(Lookahead n) = lookahead best xs

        -- WARNING: The GAP docs don't mention this but FS takes the chunking
        -- out to the end of the whole kilometer in which the task ends and
        -- then adds another 100m for good measure.
        dKm :: Quantity Double [u| km |]
        dKm =
            let whole = MkQuantity . fromIntegral $ (ceiling bd :: Integer)
            in whole +: convert [u| 100 m |]

        dN = PilotDistance dKm
        ixN = toIxChunk dN

        d0 = PilotDistance [u| 0 km |]
        ix0 = toIxChunk d0

        ixs = [ix0 .. ixN]

        -- The following snippets labelled A & B are from scoring tasks #1 & #7
        -- from the QuestAir Open competition, 2016-05-07 to 2016-05-13,
        -- Groveland, Florida, USA.
        -- https://airtribune.com/2016-quest-air-open-national-championships/results

        -- The indices of the chunks in which pilots landed out. More than one
        -- pilot can landout in the same chunk.
        zs = chunkLandouts xs

        ys :: [IxChunk]
        ys = nub zs

        starts = toChunk <$> ys
        nubStarts = nub starts

        ends = toChunk . (\(IxChunk x) -> IxChunk $ x + 1) <$> ys
        nubEnds = nub ends

        endsAhead = toChunk . (\(IxChunk x) -> IxChunk $ x + n + 1) <$> ys
        nubEndsAhead = nub endsAhead

        ns :: [(IxChunk, Int)]
        ns = sumLandouts zs

        vMap :: Map.Map IxChunk Int
        vMap = Map.fromList ns

        -- Sum the number of landouts in the next so many chunks to lookahead.
        downList = (\y -> (y, sumMap ahead vMap y)) <$> ys
        downMap = Map.fromList downList

        listOfAll = (\j -> (j, sumMap ahead vMap j)) <$> ixs

        listOfDiffs = scanl1 (\(_, b) (c, d) -> (c, b + d)) listOfAll

        lookaheadMap :: Map.Map IxChunk Integer
        lookaheadMap = toInteger <$> Map.fromList listOfAll

        sumOfDiff :: Integer
        sumOfDiff = toInteger . sum . take 1 . sortOn Down $ snd <$> listOfDiffs

        relativeDiffMap :: Map.Map IxChunk Double
        relativeDiffMap = (\d -> fromRational $ d % (2 * sumOfDiff)) <$> lookaheadMap

        relList = sortOn fst $ Map.toList relativeDiffMap
        sumRels = scanl1 (\(_, b) (c, d) -> (c, b + d)) relList

        fracMap = Map.intersection (Map.fromList sumRels) downMap

        relMap = Map.intersection (Map.fromList relList) downMap

sumMap :: Lookahead -> Map.Map IxChunk Int -> IxChunk -> Int
sumMap (Lookahead n) nMap (IxChunk ic) =
    sum $ Map.elems filtered
    where
        -- NOTE: If n = 1, we want to look 100m ahead. We want to look
        -- in chunk ic but not in chunk ic + 1.
        kMin = ic
        kMax = ic + n

        filtered =
            Map.filterWithKey
                (\(IxChunk k) _ -> kMin <= k && k < kMax)
                nMap
