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
    , collectDowns
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
import qualified Data.Map as Map

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))
import Flight.Gap.Distance.Relative (RelativeDifficulty(..))
import Flight.Gap.Fraction.Difficulty (DifficultyFraction(..))
import Flight.Gap.Distance.Pilot (PilotDistance(..))
import Flight.Gap.Distance.Min (MinimumDistance(..))
import Flight.Gap.Distance.Stop (FlownMax(..))
import Flight.Gap.Pilots (Pilot)

-- | The index of a 100m chunk. The zeroth chunk is any distance less than or
-- equal to minimum distance.
newtype IxChunk = IxChunk Int
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (Enum, Num)

instance Show IxChunk where
    show (IxChunk x) = show x

newtype Chunk a = Chunk a
    deriving (Eq, Ord)

instance Show a => Show (Chunk a) where
    show (Chunk x) = show x

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

instance (ToJSON (Chunk a)) => ToJSON (Chunks a)
instance (FromJSON (Chunk a)) => FromJSON (Chunks a)

-- | A sequence of chunk ends, distances on course in km.
newtype Chunks a = Chunks [Chunk a]
    deriving (Eq, Ord, Show, Generic)

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
        , downs :: [PilotDistance (Quantity Double [u| km |])]
        , downers :: [Pilot]
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ChunkDifficulty =
    ChunkDifficulty
        { chunk :: IxChunk
        , startChunk :: Chunk (Quantity Double [u| km |])
        , endChunk :: Chunk (Quantity Double [u| km |])
        , endAhead :: Chunk (Quantity Double [u| km |])
        , down :: Int
        , downs :: [PilotDistance (Quantity Double [u| km |])]
        , downers :: [Pilot]
        , downward :: Int
        , rel :: RelativeDifficulty
        , frac :: DifficultyFraction
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

mergeChunks
    :: [ChunkLandings] -- ^ Landings in each chunk
    -> [(IxChunk, Chunk (Quantity Double [u| km |]))] -- ^ Start of each chunk
    -> [(IxChunk, Chunk (Quantity Double [u| km |]))] -- ^ End of each chunk
    -> [(IxChunk, Chunk (Quantity Double [u| km |]))] -- ^ End of ahead chunk
    -> [ChunkLandings] -- ^ Landings summed over the lookahead
    -> [ChunkRelativeDifficulty]
    -> [ChunkDifficultyFraction]
    -> [ChunkDifficulty]
mergeChunks ls is js ks as rs ds =
    catMaybes
    [ overlay l i j k a r d
    | l <- ls
    | i <- is
    | j <- js
    | k <- ks
    | a <- as
    | r <- rs
    | d <- ds
    ]

overlay
    :: ChunkLandings
    -> (IxChunk, Chunk (Quantity Double [u| km |]))
    -> (IxChunk, Chunk (Quantity Double [u| km |]))
    -> (IxChunk, Chunk (Quantity Double [u| km |]))
    -> ChunkLandings
    -> ChunkRelativeDifficulty
    -> ChunkDifficultyFraction
    -> Maybe ChunkDifficulty
overlay
    ChunkLandings{chunk = l, down, downs, downers}
    (i, iStart)
    (j, jEnd)
    (k, kEnd)
    ChunkLandings{chunk = a, down = ahead}
    ChunkRelativeDifficulty{chunk = r, rel}
    ChunkDifficultyFraction{chunk = d, frac}
        | (l == i)
        && (i == j)
        && (j == a)
        && (a == k)
        && (k == r)
        && (r == d) =
          Just ChunkDifficulty
              { chunk = l
              , startChunk = iStart
              , endChunk = jEnd
              , endAhead = kEnd
              , down = down
              , downs = downs
              , downers = downers
              , downward = ahead
              , rel = rel
              , frac = frac
              }
        | otherwise = Nothing

-- | How many 100 m chunks to look ahead when working out the distance
-- difficulty.
lookahead
    :: FlownMax (Quantity Double [u| km |])
    -> [PilotDistance (Quantity Double [u| km |])]
    -> Lookahead
lookahead (FlownMax (MkQuantity best)) xs =
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
    -> FlownMax (Quantity Double [u| km |])
    -> Chunks (Quantity Double [u| km |])
chunks (MinimumDistance md) (FlownMax best) =
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
toChunk (MinimumDistance md) (IxChunk ix) =
    Chunk $ d +: md
    where
        d :: Quantity Double [u| km |]
        d = convert $ fromIntegral ix *: [u| 1 hm |]

-- | Converts from pilot distance to distance in units of the number of 100m
-- chunks offset from the minium distance set for the competition.
toIxChunk
    :: MinimumDistance (Quantity Double [u| km |])
    -> PilotDistance (Quantity Double [u| km |])
    -> IxChunk
toIxChunk (MinimumDistance md) (PilotDistance d) =
    IxChunk $ ceiling x
    where
        MkQuantity x = convert (d -: md) :: Quantity _ [u| hm |]

-- | In each 100m chunk where pilots landed out, how many pilots landed in that
-- chunk.
landouts
    :: MinimumDistance (Quantity Double [u| km |])
    -> [Pilot]
    -> [PilotDistance (Quantity Double [u| km |])]
    -> [ChunkLandings]
landouts md ps xs =
    collectDowns md ps xs $ sumLandouts (chunkLandouts md xs)

toIxDowns
    :: MinimumDistance (Quantity Double [u| km |])
    -> [Pilot]
    -> [PilotDistance (Quantity Double [u| km |])]
    -> Map.Map IxChunk [(Pilot, PilotDistance (Quantity Double [u| km |]))]
toIxDowns md ps xs =
    Map.fromListWith (++)
    $ zipWith (\p x -> (toIxChunk md x, [(p, x)])) ps xs

collectDowns
    :: MinimumDistance (Quantity Double [u| km |])
    -> [Pilot]
    -> [PilotDistance (Quantity Double [u| km |])]
    -> [(IxChunk, Int)]
    -> [ChunkLandings]
collectDowns md ps xs =
    fmap (\(ix, n) ->
        let (ps', ds) = unzip $ Map.findWithDefault [] ix dss
        in ChunkLandings ix n ds ps')
    where
        dss = toIxDowns md ps xs

sumLandouts:: [IxChunk] -> [(IxChunk, Int)]
sumLandouts = fmap (\gXs@(gX : _) -> (gX, length gXs)) . group

chunkLandouts
    :: MinimumDistance (Quantity Double [u| km |])
    -> [PilotDistance (Quantity Double [u| km |])]
    -> [IxChunk]
chunkLandouts md xs =
    toIxChunk md <$> sort xs
