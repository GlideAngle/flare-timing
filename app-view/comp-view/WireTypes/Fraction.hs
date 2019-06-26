module WireTypes.Fraction
    ( ReachFraction(..)
    , EffortFraction(..)
    , DistanceFraction(..)
    , SpeedFraction(..)
    , ArrivalFraction(..)
    , LeadingFraction(..)
    , Fractions(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))

newtype ReachFraction = ReachFraction Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype EffortFraction = EffortFraction Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype DistanceFraction = DistanceFraction Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype SpeedFraction = SpeedFraction Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype ArrivalFraction = ArrivalFraction Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype LeadingFraction = LeadingFraction Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data Fractions =
    Fractions
        { reach :: ReachFraction
        , effort :: EffortFraction
        , distance :: DistanceFraction
        , leading :: LeadingFraction
        , arrival :: ArrivalFraction
        , time :: SpeedFraction
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

