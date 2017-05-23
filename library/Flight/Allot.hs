{-# lANGUAGE PatternSynonyms #-}
{-# lANGUAGE ScopedTypeVariables #-}
module Flight.Allot
    ( PilotsAtEss(..)
    , PositionAtEss(..)
    , ArrivalFraction(..)
    , arrivalFraction
    , BestTime(..)
    , PilotTime(..)
    , SpeedFraction(..)
    , speedFraction
    , BestDistance(..)
    , PilotDistance(..)
    , LinearFraction(..)
    , linearFraction
    , LookaheadChunks(..)
    , lookaheadChunks
    , DifficultyFraction(..)
    , difficultyFraction
    ) where

import Data.Ratio ((%))
import Data.List (sort, group, scanl)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Flight.Ratio (pattern (:%))

newtype PilotsAtEss = PilotsAtEss Integer deriving (Eq, Show)
newtype PositionAtEss = PositionAtEss Integer deriving (Eq, Show)
newtype ArrivalFraction = ArrivalFraction Rational deriving (Eq, Ord, Show)

newtype BestTime = BestTime Rational deriving (Eq, Ord, Show)
newtype PilotTime = PilotTime Rational deriving (Eq, Ord, Show)
newtype SpeedFraction = SpeedFraction Rational deriving (Eq, Ord, Show)

newtype BestDistance = BestDistance Rational deriving (Eq, Ord, Show)
newtype PilotDistance= PilotDistance Rational deriving (Eq, Ord, Show)
newtype LinearFraction = LinearFraction Rational deriving (Eq, Ord, Show)

newtype ChunkedDistance = ChunkedDistance Integer deriving (Eq, Ord, Show)
newtype LookaheadChunks = LookaheadChunks Integer deriving (Eq, Ord, Show)
newtype DifficultyFraction = DifficultyFraction Rational deriving (Eq, Ord, Show)

arrivalFraction :: PilotsAtEss -> PositionAtEss -> ArrivalFraction
arrivalFraction (PilotsAtEss n) (PositionAtEss rank)
    | n <= 0 =
        ArrivalFraction (0 % 1)
    | rank <= 0 =
        ArrivalFraction (0 % 1)
    | rank > n =
        ArrivalFraction (0 % 1)
    | otherwise =
        ArrivalFraction $
        (2 % 10)
        + (37 % 1000) * ac
        + (13 % 100) * ac * ac
        + (633 % 1000) * ac * ac * ac
        where
        ac = (1 % 1) - ((rank - 1) % n)

speedFraction :: BestTime -> PilotTime -> SpeedFraction
speedFraction (BestTime best) (PilotTime t) =
    SpeedFraction $ max (0 % 1) sf
    where
        numerator = fromRational $ t - best :: Double
        denominator = fromRational best ** (1 / 2)
        frac = (numerator / denominator) ** (2 / 3)
        sf = (1 % 1) - toRational frac

linearFraction :: BestDistance -> PilotDistance -> LinearFraction
linearFraction (BestDistance (nb :% db)) (PilotDistance (np :% dp)) =
    LinearFraction $ (np * db) % (dp * nb)

lookaheadChunks :: BestDistance -> [PilotDistance] -> LookaheadChunks
lookaheadChunks (BestDistance (n :% d)) xs =
    LookaheadChunks $ max 30 rounded
    where
        pilotsLandedOut :: Integer
        pilotsLandedOut = toInteger $ length xs

        rounded :: Integer
        rounded = round $ (30 * n) % (pilotsLandedOut * d)

toChunk :: PilotDistance -> ChunkedDistance
toChunk (PilotDistance d) =
    ChunkedDistance $ round (d * (10 % 1))

difficultyFraction :: LookaheadChunks -> [PilotDistance] -> [DifficultyFraction]
difficultyFraction lookahead xs =
    zipWith (diffByChunk diffScoreMap) xs' ys
    where
        xs' :: [PilotDistance]
        xs' = sort xs

        ys :: [ChunkedDistance]
        ys = toChunk <$> xs'

        gs :: [[ChunkedDistance]]
        gs = group ys

        ns :: [(ChunkedDistance, Int)]
        ns = (\gXs@(gX : _) -> (gX, length gXs)) <$> gs

        nMap :: Map.Map ChunkedDistance Int
        nMap = Map.fromList ns

        lookaheadMap :: Map.Map ChunkedDistance Integer
        lookaheadMap = difficulty lookahead nMap

        sumOfDiff :: Integer
        sumOfDiff = toInteger $ sum $ Map.elems lookaheadMap

        relativeDiffMap :: Map.Map ChunkedDistance Rational
        relativeDiffMap = (\d -> d % (2 * sumOfDiff)) <$> lookaheadMap

        -- TODO: If distance > best flown * 10 then diff score is 0.5.
        diffScoreMap :: Map.Map ChunkedDistance Rational
        diffScoreMap =
            snd $ Map.mapAccum (\acc x -> (acc + x, x)) 0 relativeDiffMap

diffByChunk:: Map.Map ChunkedDistance Rational
              -> PilotDistance
              -> ChunkedDistance
              -> DifficultyFraction
diffByChunk diffMap (PilotDistance (n :% d)) pc@(ChunkedDistance pdChunk) =
    DifficultyFraction $
    pDiff
    + (pDiffNext - pDiff)
    * (((10 * n) % d) - (pdChunk % 1))
    where
        pDiff :: Rational
        pDiff = fromMaybe (0 % 1) $ Map.lookup pc diffMap

        (_, pDiffNext :: Rational) =
            fromMaybe (ChunkedDistance 0, (0 % 1)) $ Map.lookupGT pc diffMap

difficulty :: LookaheadChunks -> Map.Map ChunkedDistance Int -> Map.Map ChunkedDistance Integer
difficulty (LookaheadChunks lookahead) nMap =
    Map.mapWithKey f iMap
    where
        iMap :: Map.Map ChunkedDistance Integer
        iMap = toInteger <$> nMap

        f :: ChunkedDistance -> Integer -> Integer
        f (ChunkedDistance cd) _ =
            toInteger $ sum $ Map.elems filtered
            where
                keySet :: Set.Set ChunkedDistance
                keySet = Set.fromList $ ChunkedDistance <$> [cd .. (cd + lookahead)]

                filtered = Map.filterWithKey (\k _ -> Set.member k keySet) iMap
