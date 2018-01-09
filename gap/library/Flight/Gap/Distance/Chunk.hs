{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Gap.Distance.Chunk
    ( Lookahead(..)
    , Chunk(..)
    , Chunks(..)
    , IxChunk(..)
    , ChunkRelativeDifficulty(..)
    , ChunkDifficultyFraction(..)
    , ChunkLandings(..)
    , lookahead
    , toChunk
    , chunks
    , landouts
    , chunkLandouts
    , sumLandouts
    ) where

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
    -> Chunks
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
