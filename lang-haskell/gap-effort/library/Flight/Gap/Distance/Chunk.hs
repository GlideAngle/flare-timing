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

import GHC.Generics (Generic)
import Data.Maybe (catMaybes)
import Data.List (sort, group)
import "newtype" Control.Newtype (Newtype(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure ((*:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified Data.Map as Map

import Flight.Units ()
import Data.Via.Scientific (DefaultDecimalPlaces(..), DecimalPlaces(..))
import Data.Via.UnitsOfMeasure (ViaQ(..))
import "flight-gap-allot" Flight.Score
    ( DifficultyFraction(..), PilotDistance(..), Pilot, FlownMax(..))
import Flight.Gap.Distance.Relative (RelativeDifficulty(..))

-- | The index of a 100m chunk. The zeroth chunk is any distance less than or
-- equal to minimum distance.
newtype IxChunk = IxChunk Int
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (Enum, Num)

instance Show IxChunk where
    show (IxChunk x) = show x

newtype Chunk a = Chunk a
    deriving (Eq, Ord, Generic)

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
    deriving (Eq, Ord, Generic)

instance Show a => Show (Chunks a) where
    show (Chunks xs) = show xs

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

-- | Pilots down in this chunk. The @downs@ and @downers@ fields can be zipped.
data ChunkLandings =
    ChunkLandings
        { chunk :: IxChunk
        , down :: Int
        -- ^ How many pilots are down in this chunk.
        , downs :: [PilotDistance (Quantity Double [u| km |])]
        -- ^ The distance each of the downers came down.
        , downers :: [Pilot]
        -- ^ The pilots that landed in this chunk.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ChunkDifficulty =
    ChunkDifficulty
        { chunk :: IxChunk
        , startChunk :: Chunk (Quantity Double [u| km |])
        -- ^ The task distance to the beginning of this chunk.
        , endChunk :: Chunk (Quantity Double [u| km |])
        -- ^ The task distance to the end of this chunk.
        , endAhead :: Chunk (Quantity Double [u| km |])
        -- ^ The task distance to the end of the chunk that we look ahead to.
        , down :: Int
        -- ^ How many pilots are down in this chunk.
        , downs :: [PilotDistance (Quantity Double [u| km |])]
        -- ^ The distance each of the downers came down.
        , downers :: [Pilot]
        -- ^ The pilots that landed in this chunk.
        , downward :: Int
        -- ^ The number of pilots that landed ahead within the lookahead distance.
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
    ChunkLandings{chunk = c, down = ahead}
    ChunkRelativeDifficulty{chunk = r, rel}
    ChunkDifficultyFraction{chunk = d, frac}
        | (l == i)
        && (i == j)
        && (j == c)
        && (c == k)
        && (k == r)
        && (r == d) =
            let (downs', downers') =
                    unzip
                    $ sort
                    $ zip downs downers
            in
                Just ChunkDifficulty
                    { chunk = l
                    , startChunk = iStart
                    , endChunk = jEnd
                    , endAhead = kEnd
                    , down = down
                    , downs = downs'
                    , downers = downers'
                    , downward = ahead
                    , rel = rel
                    , frac = frac
                    }
        | otherwise = Nothing

-- | How many 100 m chunks to look ahead when working out the distance
-- difficulty.
--
-- >>> lookahead (FlownMax [u| 0 km |]) []
-- Lookahead 30
--
-- >>> lookahead (FlownMax [u| 100 km |]) (take 100 $ PilotDistance . MkQuantity <$> [0 .. ])
-- Lookahead 30
--
-- >>> lookahead (FlownMax [u| 100 km |]) (take 10 $ PilotDistance . MkQuantity <$> [0 .. ])
-- Lookahead 300
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
--
-- >>> chunks (FlownMax [u| 0 km |])
-- [[u| 0.0 km |]]
--
-- >>> chunks (FlownMax [u| 0.1 km |])
-- [[u| 0.0 km |],[u| 0.1 km |]]
--
-- >>> chunks (FlownMax [u| 0.3 km |])
-- [[u| 0.0 km |],[u| 0.1 km |],[u| 0.2 km |],[u| 0.30000000000000004 km |]]
chunks
    :: FlownMax (Quantity Double [u| km |])
    -> Chunks (Quantity Double [u| km |])
chunks (FlownMax best) =
    Chunks $ Chunk . MkQuantity <$> [x0, x1 .. xN]
    where
        MkQuantity x0 = [u| 0 km |] :: Quantity Double [u| km |]
        MkQuantity x1 = convert [u| 100m |] :: Quantity Double [u| km |]
        MkQuantity xN = best

-- | Converts from a chunk index, a number of 100m chunks offset to the start of
-- the chunks range.
--
-- prop> \x -> toChunk (IxChunk x) >= Chunk [u| 0 km |]
--
-- >>> toChunk (IxChunk 0)
-- [u| 0.0 km |]
--
-- >>> toChunk (IxChunk 1)
-- [u| 0.1 km |]
--
-- >>> toChunk (IxChunk 10)
-- [u| 1.0 km |]
toChunk :: IxChunk -> Chunk (Quantity Double [u| km |])
toChunk (IxChunk ix) =
    Chunk d
    where
        d :: Quantity Double [u| km |]
        d = convert $ fromIntegral (max 0 ix) *: [u| 1 hm |]

-- | Converts from pilot distance a chunk index.
--
-- prop> \x -> toIxChunk (PilotDistance $ MkQuantity x) >= IxChunk 0
--
-- prop> \x -> let Chunk y = toChunk (IxChunk x) in toIxChunk (PilotDistance y) == IxChunk (max 0 x)
--
-- >>> toIxChunk $ PilotDistance [u| 0 km |]
-- 0
--
-- >>> toIxChunk $ PilotDistance [u| 0.1 km |]
-- 1
--
-- >>> toIxChunk $ PilotDistance [u| -0.1 km |]
-- 0
--
-- >>> toIxChunk $ PilotDistance [u| 1 km |]
-- 10
--
-- >>> toIxChunk $ PilotDistance [u| 1.4 km |]
-- 14
toIxChunk :: PilotDistance (Quantity Double [u| km |]) -> IxChunk
toIxChunk (PilotDistance d) =
    IxChunk . max 0 $ floor x
    where
        MkQuantity x = convert d :: Quantity _ [u| hm |]

-- | In each 100m chunk where pilots landed out, how many pilots landed in that
-- chunk.
landouts
    :: [Pilot]
    -> [PilotDistance (Quantity Double [u| km |])]
    -> [ChunkLandings]
landouts ps xs =
    collectDowns ps xs $ sumLandouts (chunkLandouts xs)

toIxDowns
    :: [Pilot]
    -> [PilotDistance (Quantity Double [u| km |])]
    -> Map.Map IxChunk [(Pilot, PilotDistance (Quantity Double [u| km |]))]
toIxDowns ps xs =
    Map.fromListWith (++)
    $ zipWith (\p x -> (toIxChunk x, [(p, x)])) ps xs

collectDowns
    :: [Pilot]
    -> [PilotDistance (Quantity Double [u| km |])]
    -> [(IxChunk, Int)]
    -> [ChunkLandings]
collectDowns ps xs =
    fmap (\(ix, n) ->
        let (ps', ds) = unzip $ Map.findWithDefault [] ix dss
        in ChunkLandings ix n ds ps')
    where
        dss = toIxDowns ps xs

sumLandouts:: [IxChunk] -> [(IxChunk, Int)]
sumLandouts = fmap (\gXs@(gX : _) -> (gX, length gXs)) . group

chunkLandouts
    :: [PilotDistance (Quantity Double [u| km |])]
    -> [IxChunk]
chunkLandouts xs =
    toIxChunk <$> sort xs
