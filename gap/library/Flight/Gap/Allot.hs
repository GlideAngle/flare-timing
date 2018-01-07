{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Gap.Allot
    ( PilotsAtEss(..)
    , PositionAtEss(..)
    , ArrivalFraction(..)
    , arrivalFraction
    , BestTime(..)
    , PilotTime(..)
    , bestTime
    , SpeedFraction(..)
    , speedFraction
    , MinimumDistance(..)
    , BestDistance(..)
    , PilotDistance(..)
    , LinearFraction(..)
    , linearFraction
    , Lookahead(..)
    , Chunk(..)
    , Chunks(..)
    , IxChunk(..)
    , lookahead
    , toChunk
    , chunks
    , landouts
    , SumOfDifficulty(..)
    , RelativeDifficulty(..)
    , DifficultyFraction(..)
    , ChunkRelativeDifficulty(..)
    , ChunkDifficultyFraction(..)
    , ChunkLandings(..)
    , Difficulty(..)
    , difficulty
    ) where

import Control.Newtype (Newtype(..))
import Data.Ratio ((%))
import qualified Data.List as List (minimum)
import Data.List (sort, sortOn, group)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure ((+:), (-:), u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import GHC.Generics (Generic)

import Flight.Gap.Ratio (pattern (:%))
import Flight.Units ()
import Data.Aeson.Via.Scientific
    (DefaultDecimalPlaces(..), DecimalPlaces(..), ViaSci(..))
import Data.Aeson.Via.UnitsOfMeasure (ViaQ(..))

newtype MinimumDistance = MinimumDistance (Quantity Double [u| km |])
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces MinimumDistance where
    defdp _ = DecimalPlaces 1

instance (u ~ [u| km |]) => Newtype MinimumDistance (Quantity Double u) where
    pack = MinimumDistance
    unpack (MinimumDistance a) = a

instance ToJSON MinimumDistance where
    toJSON x = toJSON $ ViaQ x

instance FromJSON MinimumDistance where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x

-- | The number of pilots completing the speed section of the task.
newtype PilotsAtEss = PilotsAtEss Integer
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | A 1-based rank of the pilot arrival at goal, 1st in is 1, 2nd is 2 etc.
newtype PositionAtEss = PositionAtEss Integer
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype ArrivalFraction = ArrivalFraction Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces ArrivalFraction where
    defdp _ = DecimalPlaces 8

instance Newtype ArrivalFraction Rational where
    pack = ArrivalFraction
    unpack (ArrivalFraction a) = a

newtype SpeedFraction = SpeedFraction Rational deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces SpeedFraction where
    defdp _ = DecimalPlaces 8

instance Newtype SpeedFraction Rational where
    pack = SpeedFraction
    unpack (SpeedFraction a) = a

-- | Best time for the task, units of hours.
newtype BestTime = BestTime Rational deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces BestTime where
    defdp _ = DecimalPlaces 8

instance Newtype BestTime Rational where
    pack = BestTime
    unpack (BestTime a) = a

-- | Pilot time for the task, units of hours.
newtype PilotTime = PilotTime Rational deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces PilotTime where
    defdp _ = DecimalPlaces 8

instance Newtype PilotTime Rational where
    pack = PilotTime
    unpack (PilotTime a) = a

newtype BestDistance = BestDistance (Quantity Double [u| km |])
    deriving (Eq, Ord, Show)

newtype PilotDistance a = PilotDistance a deriving (Eq, Ord, Show)
newtype LinearFraction = LinearFraction Rational deriving (Eq, Ord, Show)

-- | The index of a 100m chunk. The zeroth chunk is any distance less than or
-- equal to minimum distance.
newtype IxChunk = IxChunk Int
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | The sum of all chunk difficulties.
newtype SumOfDifficulty = SumOfDifficulty Integer
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | The relative difficulty of a chunk.
newtype RelativeDifficulty = RelativeDifficulty Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces RelativeDifficulty where
    defdp _ = DecimalPlaces 8

instance Newtype RelativeDifficulty Rational where
    pack = RelativeDifficulty
    unpack (RelativeDifficulty a) = a

instance ToJSON RelativeDifficulty where
    toJSON x = toJSON $ ViaSci x

instance FromJSON RelativeDifficulty where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x

-- | The sum of relative difficulties up until the chunk of landing.
newtype DifficultyFraction = DifficultyFraction Rational
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces DifficultyFraction where
    defdp _ = DecimalPlaces 8

instance Newtype DifficultyFraction Rational where
    pack = DifficultyFraction
    unpack (DifficultyFraction a) = a

instance ToJSON DifficultyFraction where
    toJSON x = toJSON $ ViaSci x

instance FromJSON DifficultyFraction where
    parseJSON o = do
        ViaSci x <- parseJSON o
        return x

-- | The relative difficulty for this chunk.
data ChunkRelativeDifficulty =
    ChunkRelativeDifficulty
        { chunk :: IxChunk
        , rel :: RelativeDifficulty
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | The difficulty fraction for this chunk.
data ChunkDifficultyFraction =
    ChunkDifficultyFraction
        { chunk :: IxChunk
        , frac :: DifficultyFraction
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | How many pilots down in this chunk.
data ChunkLandings =
    ChunkLandings
        { chunk :: IxChunk
        , down :: Int
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Difficulty =
    Difficulty
        { sumOf :: SumOfDifficulty
        , relative :: [ChunkRelativeDifficulty]
        , fractional :: [ChunkDifficultyFraction]
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | How far to look ahead, in units of 100m chunks.
newtype Lookahead = Lookahead Int
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype Chunk = Chunk (Quantity Double [u| km |])
    deriving (Eq, Ord, Show)

instance DefaultDecimalPlaces Chunk where
    defdp _ = DecimalPlaces 1

instance (u ~ [u| km |]) => Newtype Chunk (Quantity Double u) where
    pack = Chunk
    unpack (Chunk a) = a

instance ToJSON Chunk where
    toJSON x = toJSON $ ViaQ x

instance FromJSON Chunk where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x

-- | A sequence of chunk ends, distances on course in km.
newtype Chunks = Chunks [Chunk]
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

arrivalFraction :: PilotsAtEss -> PositionAtEss -> ArrivalFraction
arrivalFraction (PilotsAtEss n) (PositionAtEss rank)
    | n <= 0 =
        ArrivalFraction 0
    | rank <= 0 =
        ArrivalFraction 0
    | rank > n =
        ArrivalFraction 0
    | otherwise =
        ArrivalFraction $
        (2 % 10)
        + (37 % 1000) * ac
        + (13 % 100) * ac * ac
        + (633 % 1000) * ac * ac * ac
        where
            ac = 1 - ((rank - 1) % n)

bestTime :: [PilotTime] -> Maybe BestTime
bestTime [] = Nothing
bestTime xs = let PilotTime t = List.minimum xs in Just $ BestTime t

speedFraction :: BestTime -> PilotTime -> SpeedFraction
speedFraction (BestTime best) (PilotTime t) =
    SpeedFraction $ max (0 % 1) sf
    where
        numerator = fromRational $ t - best :: Double
        denominator = fromRational best ** (1 / 2)
        frac = (numerator / denominator) ** (2 / 3)
        sf = (1 % 1) - toRational frac

-- | The linear fraction for distance.
linearFraction
    :: BestDistance
    -> PilotDistance (Quantity Double [u| km |]) 
    -> LinearFraction
linearFraction (BestDistance bd) (PilotDistance pd) =
    LinearFraction $ (np * db) % (dp * nb)
    where
        MkQuantity (nb :% db) = toRational' bd
        MkQuantity (np :% dp) = toRational' pd

-- | How many 100 m chunks to look ahead when working out the distance
-- difficulty.
lookahead
    :: BestDistance
    -> [PilotDistance (Quantity Double [u| km |])]
    -> Lookahead
lookahead (BestDistance (MkQuantity best)) xs =
    Lookahead . max 30 $
    if null xs then 0
               else round ((30 * best) / fromInteger pilotsLandedOut)
    where
        pilotsLandedOut = toInteger $ length xs

-- | A list of 100m chunks of distance starting from the minimum distance set
-- up for the competition. Pilots that fly less than minimum distance get
-- awarded that distance.
chunks :: MinimumDistance -> BestDistance -> Chunks
chunks (MinimumDistance md) (BestDistance best) =
    Chunks $ Chunk . MkQuantity <$> [x0, x1 .. xN]
    where
        MkQuantity x0 = md
        MkQuantity x1 = md +: convert [u| 100m |]
        MkQuantity xN = best

-- | Converts from pilot distance to distance in units of the number of 100m
-- chunks offset from the minium distance set for the competition.
toChunk
    :: MinimumDistance
    -> PilotDistance (Quantity Double [u| km |])
    -> IxChunk
toChunk (MinimumDistance md) (PilotDistance d)
    | d <= md = IxChunk 0
    | otherwise = IxChunk $ ceiling x
    where
        MkQuantity x = convert (d -: md) :: Quantity _ [u| hm |]

-- | In each 100m chunk where pilots landed out, how many pilots landed in that
-- chunk.
landouts
    :: MinimumDistance
    -> [PilotDistance (Quantity Double [u| km |])]
    -> [ChunkLandings]
landouts md xs =
    uncurry ChunkLandings
    <$> sumLandouts (chunkLandouts md xs)

sumLandouts :: [IxChunk] -> [(IxChunk, Int)]
sumLandouts = fmap (\gXs@(gX : _) -> (gX, length gXs)) . group

chunkLandouts
    :: MinimumDistance
    -> [PilotDistance (Quantity Double [u| km |])]
    -> [IxChunk]
chunkLandouts md xs =
    toChunk md <$> sort xs

-- | For a list of distances flown by pilots, works out the distance difficulty
-- fraction for each pilot.
difficulty
    :: MinimumDistance
    -> BestDistance
    -> [PilotDistance (Quantity Double [u| km |])]
    -> Difficulty
difficulty md best xs =
    Difficulty
        { sumOf = SumOfDifficulty sumOfDiff
        , relative = relatives
        , fractional = fractionals
        }
    where
        ys :: [IxChunk]
        ys = chunkLandouts md xs

        ns :: [(IxChunk, Int)]
        ns = sumLandouts ys

        nMap :: Map.Map IxChunk Int
        nMap = Map.fromList ns

        n = lookahead best xs

        lookaheadMap :: Map.Map IxChunk Integer
        lookaheadMap = toInteger <$> lookaheadDifficulty md n nMap

        sumOfDiff :: Integer
        sumOfDiff = toInteger $ sum $ Map.elems lookaheadMap

        relativeDiffMap :: Map.Map IxChunk Rational
        relativeDiffMap = (\d -> d % (2 * sumOfDiff)) <$> lookaheadMap

        rels = sortOn fst $ Map.toList relativeDiffMap
        fracs = scanl1 (\(_, b) (c, d) -> (c, b + d)) rels

        relatives =
            (\(a, b) -> ChunkRelativeDifficulty a (RelativeDifficulty b))
            <$> rels

        fractionals =
            (\(a, b) -> ChunkDifficultyFraction a (DifficultyFraction b))
            <$> fracs

diffByChunk
    :: Map.Map IxChunk Rational
    -> PilotDistance (Quantity Double [u| km |])
    -> IxChunk
    -> DifficultyFraction
diffByChunk diffMap (PilotDistance (MkQuantity pd)) pc@(IxChunk pdChunk) =
    DifficultyFraction $
    pDiff
    + (pDiffNext - pDiff)
    * (((10 * n) % d) - (toInteger pdChunk % 1))
    where
        (n :% d) = toRational pd
        pDiff :: Rational
        pDiff = fromMaybe (0 % 1) $ Map.lookup pc diffMap

        (_, pDiffNext :: Rational) =
            fromMaybe (IxChunk 0, 0 % 1) $ Map.lookupGT pc diffMap

lookaheadDifficulty
    :: MinimumDistance
    -> Lookahead
    -> Map.Map IxChunk Int
    -> Map.Map IxChunk Int
lookaheadDifficulty _ (Lookahead n) nMap =
    Map.mapWithKey f nMap
    where
        f :: IxChunk -> Int -> Int
        f (IxChunk cd) _ =
            sum $ Map.elems filtered
            where
                kMin = cd
                kMax = cd + n

                filtered =
                    Map.filterWithKey
                        (\(IxChunk k) _ -> kMin <= k && k <= kMax)
                        nMap
