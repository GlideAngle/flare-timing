module WireTypes.Effort
    ( TrackEffort(..)
    , TaskLanding(..)
    , IxChunk(..)
    , Lookahead(..)
    , Chunk(..)
    , SumOfDifficulty(..)
    , Chunking(..)
    , RelativeDifficulty(..)
    , DifficultyFraction(..)
    , ChunkDifficulty(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))

import WireTypes.Pilot (Pilot(..))
import WireTypes.Point (PilotDistance(..))
import WireTypes.Fraction (EffortFraction(..))

data TrackEffort =
    TrackEffort
        { effort :: PilotDistance
        , frac :: EffortFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data TaskLanding =
    TaskLanding
        { minDistance :: PilotDistance
        , bestDistance :: Maybe PilotDistance
        , landout :: Int
        , lookahead :: Maybe Lookahead
        , chunking :: Maybe Chunking
        , difficulty :: Maybe [ChunkDifficulty]
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype IxChunk = IxChunk Int
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)
    deriving newtype (Enum, Num)

newtype Lookahead = Lookahead Int
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype Chunk = Chunk PilotDistance
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype SumOfDifficulty = SumOfDifficulty Integer
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data Chunking =
    Chunking
        { sumOf :: SumOfDifficulty
        , startChunk :: (IxChunk, Chunk)
        , endChunk :: (IxChunk, Chunk)
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype RelativeDifficulty = RelativeDifficulty Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype DifficultyFraction = DifficultyFraction Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data ChunkDifficulty =
    ChunkDifficulty
        { chunk :: IxChunk
        , startChunk :: Chunk
        , endChunk :: Chunk
        , endAhead :: Chunk
        , down :: Int
        , downs :: [PilotDistance]
        , downers :: [Pilot]
        , downward :: Int
        , rel :: RelativeDifficulty
        , frac :: DifficultyFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

