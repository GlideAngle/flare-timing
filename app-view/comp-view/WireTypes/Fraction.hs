module WireTypes.Fraction
    ( ReachFraction(..)
    , EffortFraction(..)
    , DistanceFraction(..)
    , SpeedFraction(..)
    , ArrivalFraction(..)
    , LeadingFraction(..)
    , Fractions(..)
    , showReachFrac, showReachFracDiff
    , showEffortFrac, showEffortFracDiff
    , showLeadingFrac, showLeadingFracDiff
    , showSpeedFrac, showSpeedFracDiff
    , showArrivalFrac, showArrivalFracDiff
    ) where

import Text.Printf (printf)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))
import qualified Data.Text as T (Text, pack)

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

showReachFrac :: ReachFraction -> T.Text
showReachFrac (ReachFraction x) = T.pack $ printf "%.3f" x

showReachFracDiff :: ReachFraction -> ReachFraction -> T.Text
showReachFracDiff (ReachFraction expected) (ReachFraction actual)
    | f actual == f expected = "="
    | otherwise = f (actual - expected)
    where
        f = T.pack . printf "%+.3f"

showLeadingFrac :: LeadingFraction -> T.Text
showLeadingFrac (LeadingFraction x) = T.pack $ printf "%.3f" x

showLeadingFracDiff :: LeadingFraction -> LeadingFraction -> T.Text
showLeadingFracDiff (LeadingFraction expected) (LeadingFraction actual)
    | f actual == f expected = "="
    | otherwise = f (actual - expected)
    where
        f = T.pack . printf "%+.3f"

showEffortFrac :: EffortFraction -> T.Text
showEffortFrac (EffortFraction x) = T.pack $ printf "%.3f" x

showEffortFracDiff :: EffortFraction -> EffortFraction -> T.Text
showEffortFracDiff (EffortFraction expected) (EffortFraction actual)
    | f actual == f expected = "="
    | otherwise = f (actual - expected)
    where
        f = T.pack . printf "%+.3f"

showSpeedFrac :: SpeedFraction -> T.Text
showSpeedFrac (SpeedFraction x) = T.pack $ printf "%.3f" x

showSpeedFracDiff :: SpeedFraction -> SpeedFraction -> T.Text
showSpeedFracDiff (SpeedFraction expected) (SpeedFraction actual)
    | f actual == f expected = "="
    | otherwise = f (actual - expected)
    where
        f = T.pack . printf "%+.3f"

showArrivalFrac :: ArrivalFraction -> T.Text
showArrivalFrac (ArrivalFraction x) = T.pack $ printf "%.3f" x

showArrivalFracDiff :: ArrivalFraction -> ArrivalFraction -> T.Text
showArrivalFracDiff (ArrivalFraction expected) (ArrivalFraction actual)
    | f actual == f expected = "="
    | otherwise = f (actual - expected)
    where
        f = T.pack . printf "%+.3f"
