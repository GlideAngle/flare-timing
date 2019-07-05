module Flight.Gap.Distance.Difficulty
    ( SumOfDifficulty(..)
    , Difficulty(..)
    , gradeDifficulty
    ) where

import Data.Ratio ((%))
import Data.Maybe (catMaybes)
import Data.List (sort, sortOn, nub)
import qualified Data.Map.Strict as Map
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import GHC.Generics (Generic)

import Flight.Units ()
import Flight.Gap.Distance.Pilot (PilotDistance(..))
import Flight.Gap.Distance.Min (MinimumDistance(..))
import Flight.Gap.Distance.Stop (FlownMax(..))
import Flight.Gap.Distance.Relative (RelativeDifficulty(..))
import Flight.Gap.Fraction.Difficulty (DifficultyFraction(..))
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
import Flight.Gap.Pilots (Pilot)

-- | The sum of all chunk difficulties.
newtype SumOfDifficulty = SumOfDifficulty Integer
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Difficulty =
    Difficulty
        { sumOf :: SumOfDifficulty
        -- ^ The sum of the downward counts.
        , startChunk :: [(IxChunk, Chunk (Quantity Double [u| km |]))]
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
    :: MinimumDistance (Quantity Double [u| km |])
    -> FlownMax (Quantity Double [u| km |])
    -> [Pilot]
    -> [PilotDistance (Quantity Double [u| km |])]
    -> Difficulty
gradeDifficulty md best@(FlownMax bd) pilots landings =
    Difficulty
        { sumOf = SumOfDifficulty sumOfDiff
        , startChunk = zip ys nubStarts
        , endChunk = zip ys nubEnds
        , endAhead = zip ys nubEndsAhead
        , downward = collectDowns md pilots xs downList
        , relative =
            catMaybes $
            (\y -> do
                rel <- Map.lookup y relMap
                let f = uncurry ChunkRelativeDifficulty . fmap (RelativeDifficulty . toRational)
                return $ f (y, rel))
            <$> ys
        , fractional =
            catMaybes $
            (\y -> do
                frac <- Map.lookup y fracMap
                let f = uncurry ChunkDifficultyFraction . fmap (DifficultyFraction . toRational)
                return $ f (y, frac))
            <$> ys
        }
    where
        -- When pilots fly away from goal looking for lift but land out they
        -- can end up with a negative distance along the course. We'll zero
        -- these landings before starting on course difficulty.
        xs = (\(PilotDistance d) -> PilotDistance $ max d [u| 0 km |]) <$> landings

        ahead@(Lookahead n) = lookahead best xs
        gd = PilotDistance bd
        gIx = toIxChunk md gd
        ix0 = toIxChunk md (PilotDistance [u| 0 km |])

        ixs = [ix0 .. gIx]

        -- The following snippets labelled A & B are from scoring tasks #1 & #7
        -- from the QuestAir Open competition, 2016-05-07 to 2016-05-13,
        -- Groveland, Florida, USA.
        -- https://airtribune.com/2016-quest-air-open-national-championships/results

        -- The indices of the chunks in which pilots landed out. More than one
        -- pilot can landout in the same chunk.
        -- A: [218,279,354,424,426,472,581,605,663,704,712,714,771,780,780,863,905,932,938,991,1248,1283,1325,1429]
        zs = chunkLandouts md xs

        -- A: [218,279,354,424,426,472,581,605,663,704,712,714,771,780,863,905,932,938,991,1248,1283,1325,1429]
        ys :: [IxChunk]
        ys = nub zs

        -- A: [[u| 26.700000000000003 km |],[u| 32.8 km |],[u| 40.300000000000004 km |],...,[u| 133.20000000000002 km |],[u| 137.4 km |],[u| 147.8 km |]]
        starts = toChunk md . (\(IxChunk x) -> IxChunk $ x - 1) <$> ys
        nubStarts = nub starts

        -- A: [[u| 26.8 km |],[u| 32.900000000000006 km |],[u| 40.4 km |],...,[u| 133.3 km |],[u| 137.5 km |],[u| 147.9 km |]]
        ends = toChunk md <$> ys
        nubEnds = nub ends

        -- A: [[u| 46.800000000000004 km |],[u| 52.900000000000006 km |],[u| 60.400000000000006 km |],...,[u| 153.3 km |],[u| 157.5 km |],[u| 167.9 km |]]
        endsAhead = toChunk md . (\(IxChunk x) -> IxChunk $ x + n) <$> ys
        nubEndsAhead = nub endsAhead

        -- A: [(218,1),(279,1),(354,1),...,(1283,1),(1325,1),(1429,1)]
        ns :: [(IxChunk, Int)]
        ns = sumLandouts zs

        vMap :: Map.Map IxChunk Int
        vMap = Map.fromList ns

        -- Sum the number of landouts in the next so many chunks to lookahead.
        -- A: [(218,3),(279,5),(354,4),...,(1283,3),(1325,2),(1429,1)]
        downList = (\y -> (y, sumMap ahead vMap y)) <$> ys
        downMap = Map.fromList downList

        -- A: [(-50,0),(-49,0),(-48,0),,...,(1549,0),(1550,0),(1551,0)]
        listOfAll = (\j -> (j, sumMap ahead vMap j)) <$> ixs

        -- A: [(0,0),(1,0),(2,0),...,(1549,4800),(1550,4800),(1551,4800)]
        listOfDiffs = scanl1 (\(_, b) (c, d) -> (c, b + d)) listOfAll

        -- A: [(0,0),(1,0),(2,0),...,(1549,0),(1550,0),(1551,0)]
        lookaheadMap :: Map.Map IxChunk Integer
        lookaheadMap = toInteger <$> Map.fromList listOfAll

        -- A: 4800
        sumOfDiff :: Integer
        sumOfDiff = toInteger . sum . take 1 . reverse . sort $ snd <$> listOfDiffs

        -- A: [(0,0.0),(1,0.0),(2,0.0),...,(1549,0.0),(1550,0.0),(1551,0.0)]
        relativeDiffMap :: Map.Map IxChunk Double
        relativeDiffMap = (\d -> fromRational $ d % (2 * sumOfDiff)) <$> lookaheadMap

        relList = sortOn fst $ Map.toList relativeDiffMap
        sumRels = scanl1 (\(_, b) (c, d) -> (c, b + d)) relList

        -- A: [(218,4.19791666666667e-2),(279,6.666666666666679e-2),(354,9.791666666666647e-2),...,(1283,0.4804166666666674),(1325,0.4891666666666664),(1429,0.5000000000000014)]
        fracMap = Map.intersection (Map.fromList sumRels) downMap

        -- A: [(218,3.125e-4),(279,5.208333333333333e-4),(354,4.166666666666667e-4),...,(1283,3.125e-4),(1325,2.0833333333333335e-4),(1429,1.0416666666666667e-4)]
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
