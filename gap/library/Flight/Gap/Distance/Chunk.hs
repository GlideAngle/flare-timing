{-# LANGUAGE DuplicateRecordFields #-}
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
    , toIxChunk
    , toChunk
    , chunks
    , landouts
    , chunkLandouts
    , sumLandouts
    , mergeChunks
    ) where

import Data.Maybe (catMaybes)
import Data.List (sort, group)
import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure ((*:), (+:), (-:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import GHC.Generics (Generic)

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))
import Flight.Gap.Distance.Relative (RelativeDifficulty(..))
import Flight.Gap.Distance.Fraction (DifficultyFraction(..))
import Flight.Gap.Distance.Linear (PilotDistance(..))
import Flight.Gap.Distance.Min (MinimumDistance(..))
import Flight.Gap.Distance.Best (BestDistance(..))

-- | The index of a 100m chunk. The zeroth chunk is any distance less than or
-- equal to minimum distance.
newtype IxChunk = IxChunk Int
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

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
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

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
        , startChunk :: Chunk (Quantity Double [u| km |])
        , endChunk :: Chunk (Quantity Double [u| km |])
        , endAhead :: Chunk (Quantity Double [u| km |])
        , down :: Int
        , downward :: Int
        , rel :: RelativeDifficulty
        , frac :: DifficultyFraction
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

mergeChunks
    :: [ChunkLandings] -- ^ Landings in each chunk
    -> [(IxChunk, Chunk (Quantity Double [u| km |]))] -- ^ Start of each chunk
    -> [(IxChunk, Chunk (Quantity Double [u| km |]))] -- ^ End of each chunk
    -> [ChunkLandings] -- ^ Landings summed over the lookahead
    -> [(IxChunk, Chunk (Quantity Double [u| km |]))] -- ^ End of ahead chunk
    -> [ChunkRelativeDifficulty]
    -> [ChunkDifficultyFraction]
    -> [ChunkDifficulty]
mergeChunks ls ils jls as jas rs ds =
    catMaybes
    [ overlay l il jl a ja r d
    | l <- ls
    | il <- ils
    | jl <- jls
    | a <- as
    | ja <- jas
    | r <- rs
    | d <- ds
    ]

overlay
    :: ChunkLandings
    -> (IxChunk, Chunk (Quantity Double [u| km |]))
    -> (IxChunk, Chunk (Quantity Double [u| km |]))
    -> ChunkLandings
    -> (IxChunk, Chunk (Quantity Double [u| km |]))
    -> ChunkRelativeDifficulty
    -> ChunkDifficultyFraction
    -> Maybe ChunkDifficulty
overlay
    ChunkLandings{chunk = l, down}
    (il, sl) 
    (jl, el) 
    ChunkLandings{chunk = a, down = ahead}
    (ja, ea) 
    ChunkRelativeDifficulty{chunk = r, rel}
    ChunkDifficultyFraction{chunk = d, frac}
        | (l == il)
        && (il == jl)
        && (jl == a)
        && (a == ja)
        && (ja == r)
        && (r == d) =
          Just ChunkDifficulty
              { chunk = l
              , startChunk = sl
              , endChunk = el
              , endAhead = ea
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

-- | Converts from a chunk, a number of 100m chunks offset from the minium
-- distance set for the competition to the end of the chunks range.
toChunk
    :: MinimumDistance (Quantity Double [u| km |])
    -> IxChunk
    -> Chunk (Quantity Double [u| km |])
toChunk (MinimumDistance md) (IxChunk ix)
    | ix <= 0 = Chunk md
    | otherwise = Chunk $ d +: md
    where
        d :: Quantity Double [u| km |]
        d = convert $ fromIntegral ix *: [u| 1 hm |]

-- | Converts from pilot distance to distance in units of the number of 100m
-- chunks offset from the minium distance set for the competition.
toIxChunk
    :: MinimumDistance (Quantity Double [u| km |])
    -> PilotDistance (Quantity Double [u| km |])
    -> IxChunk
toIxChunk (MinimumDistance md) (PilotDistance d)
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
    toIxChunk md <$> sort xs
