{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Gap.Distance.Chunk
    ( Lookahead(..)
    , Chunk(..)
    , Chunks(..)
    , IxChunk(..)
    , ChunkRelativeDifficulty(..)
    , ChunkDifficultyFraction(..)
    , ChunkLandings(..)
    , ChunkDifficulty(..)
    , lookahead
    , toChunk
    , chunks
    , landouts
    , chunkLandouts
    , sumLandouts
    , mergeChunks
    ) where

import Data.Maybe (catMaybes)
import Data.List (sort, group)
import Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure ((+:), (-:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import GHC.Generics (Generic)

import Flight.Units ()
import Data.Aeson.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Aeson.Via.UnitsOfMeasure (ViaQ(..))
import Flight.Gap.Distance.Relative (RelativeDifficulty(..))
import Flight.Gap.Distance.Fraction (DifficultyFraction(..))
import Flight.Gap.Distance.Linear (BestDistance(..), PilotDistance(..))
import Flight.Gap.Distance.Min (MinimumDistance(..))

-- | The index of a 100m chunk. The zeroth chunk is any distance less than or
-- equal to minimum distance.
newtype IxChunk = IxChunk Int
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype Chunk a = Chunk a
    deriving (Eq, Ord, Show)

instance (q ~ Quantity Double [u| km |]) => DefaultDecimalPlaces (Chunk q) where
    defdp _ = DecimalPlaces 1

instance (q ~ Quantity Double [u| km |]) => Newtype (Chunk q) q where
    pack = Chunk
    unpack (Chunk a) = a

instance (q ~ Quantity Double [u| km |]) => ToJSON (Chunk q) where
    toJSON x = toJSON $ ViaQ x

instance (q ~ Quantity Double [u| km |]) => FromJSON (Chunk q) where
    parseJSON o = do
        ViaQ x <- parseJSON o
        return x

-- | A sequence of chunk ends, distances on course in km.
newtype Chunks a = Chunks [Chunk a]
    deriving (Eq, Ord, Show, Generic)

instance (ToJSON (Chunk a)) => ToJSON (Chunks a)
instance (FromJSON (Chunk a)) => FromJSON (Chunks a)

-- | How far to look ahead, in units of 100m chunks.
newtype Lookahead = Lookahead Int
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

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

data ChunkDifficulty =
    ChunkDifficulty
        { chunk :: IxChunk
        , down :: Int
        , downward :: Int
        , rel :: RelativeDifficulty
        , frac :: DifficultyFraction
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

mergeChunks
    :: [ChunkLandings] -- ^ Landings in each chunk
    -> [ChunkLandings] -- ^ Landings summed over the lookahead
    -> [ChunkRelativeDifficulty]
    -> [ChunkDifficultyFraction]
    -> [ChunkDifficulty]
mergeChunks ls as rs ds =
    catMaybes
    [ overlay l a r d
    | l <- ls
    | a <- as
    | r <- rs
    | d <- ds
    ]

overlay
    :: ChunkLandings
    -> ChunkLandings
    -> ChunkRelativeDifficulty
    -> ChunkDifficultyFraction
    -> Maybe ChunkDifficulty
overlay
    ChunkLandings{chunk = l, down}
    ChunkLandings{chunk = a, down = ahead}
    ChunkRelativeDifficulty{chunk = r, rel}
    ChunkDifficultyFraction{chunk = d, frac}
        | (l == a) && (a == r) && (r == d) =
          Just ChunkDifficulty
              { chunk = l
              , down = down
              , downward = ahead
              , rel = rel
              , frac = frac
              }
        | otherwise = Nothing

-- | How many 100 m chunks to look ahead when working out the distance
-- difficulty.
lookahead
    :: BestDistance (Quantity Double [u| km |])
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
chunks
    :: MinimumDistance (Quantity Double [u| km |])
    -> BestDistance (Quantity Double [u| km |])
    -> Chunks (Quantity Double [u| km |])
chunks (MinimumDistance md) (BestDistance best) =
    Chunks $ Chunk . MkQuantity <$> [x0, x1 .. xN]
    where
        MkQuantity x0 = md
        MkQuantity x1 = md +: convert [u| 100m |]
        MkQuantity xN = best

-- | Converts from pilot distance to distance in units of the number of 100m
-- chunks offset from the minium distance set for the competition.
toChunk
    :: MinimumDistance (Quantity Double [u| km |])
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
    :: MinimumDistance (Quantity Double [u| km |])
    -> [PilotDistance (Quantity Double [u| km |])]
    -> [ChunkLandings]
landouts md xs =
    uncurry ChunkLandings
    <$> sumLandouts (chunkLandouts md xs)

sumLandouts :: [IxChunk] -> [(IxChunk, Int)]
sumLandouts = fmap (\gXs@(gX : _) -> (gX, length gXs)) . group

chunkLandouts
    :: MinimumDistance (Quantity Double [u| km |])
    -> [PilotDistance (Quantity Double [u| km |])]
    -> [IxChunk]
chunkLandouts md xs =
    toChunk md <$> sort xs
