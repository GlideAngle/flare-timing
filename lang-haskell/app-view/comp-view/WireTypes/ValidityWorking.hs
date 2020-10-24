{-# LANGUAGE DuplicateRecordFields #-}

module WireTypes.ValidityWorking
    ( ValidityWorking(..)
    -- * Launch Validity Working
    , LaunchValidityWorking(..)
    , PilotsPresent(..)
    , PilotsFlying(..)
    , NominalLaunch(..)
    , showPilotsPresentDiff
    , showPilotsFlyingDiff
    , showNominalLaunchDiff
    -- * Distance Validity Working
    , DistanceValidityWorking(..)
    , SumOfDistance(..)
    , NominalDistanceArea(..)
    , NominalGoal(..)
    , NominalDistance(..)
    , MinimumDistance(..)
    , showNominalGoal, showNominalGoalDiff
    , showSumOfDistance, showSumOfDistanceDiff
    , showNominalDistance, showNominalDistanceDiff
    , showNominalDistanceArea, showNominalDistanceAreaDiff
    , showMinimumDistance, showMinimumDistanceDiff
    -- * Time Validity Working
    , TimeValidityWorking(..)
    , BestTime(..)
    , NominalTime(..)
    , showBestTime, showBestTimeDiff
    , showNominalTime, showNominalTimeDiff
    -- * Stop Validity Working
    , ReachStats(..)
    , StopValidityWorking(..)
    , PilotsAtEss(..)
    , PilotsLanded(..)
    , LaunchToEss(..)
    , showLaunchToEss, showLaunchToEssDiff
    , showPilotsLandedDiff
    , showPilotsAtEssDiff
    ) where

import Text.Printf (printf)
import Control.Applicative (empty)
import GHC.Generics (Generic)
import qualified Data.Text as T (Text, pack, unpack)
import Data.Aeson (Value(..), FromJSON(..))

import FlareTiming.Time (showHmsForHours, showNominalTDiff)
import WireTypes.Point (PilotDistance(..), ReachToggle(..))

data ValidityWorking =
    ValidityWorking
        { launch :: LaunchValidityWorking
        , distance :: DistanceValidityWorking
        , time :: TimeValidityWorking
        , stop :: Maybe StopValidityWorking
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data LaunchValidityWorking =
    LaunchValidityWorking
        { flying :: PilotsFlying
        , present :: PilotsPresent
        , nominalLaunch :: NominalLaunch
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype PilotsPresent = PilotsPresent Integer
    deriving (Eq, Ord)
    deriving newtype (Show, FromJSON)

newtype PilotsFlying = PilotsFlying Integer
    deriving (Eq, Ord)
    deriving newtype (Show, FromJSON)

showPilotsPresentDiff :: PilotsPresent -> PilotsPresent -> T.Text
showPilotsPresentDiff (PilotsPresent expected) (PilotsPresent actual)
    | f actual == f expected = "="
    | otherwise = T.pack . f $ actual - expected
    where
        f = printf "%+d"

showPilotsFlyingDiff :: PilotsFlying -> PilotsFlying -> T.Text
showPilotsFlyingDiff (PilotsFlying expected) (PilotsFlying actual)
    | f actual == f expected = "="
    | otherwise = T.pack . f $ actual - expected
    where
        f = printf "%+d"

showNominalLaunchDiff :: NominalLaunch -> NominalLaunch -> T.Text
showNominalLaunchDiff (NominalLaunch expected) (NominalLaunch actual)
    | f actual == f expected = "="
    | otherwise = T.pack . f $ actual - expected
    where
        f = printf "%+.2f"

newtype NominalLaunch = NominalLaunch Double
    deriving (Eq, Ord)
    deriving newtype (Show, FromJSON)

data DistanceValidityWorking =
    DistanceValidityWorking
        { sum :: SumOfDistance
        , flying :: PilotsFlying
        , area :: NominalDistanceArea
        , nominalGoal :: NominalGoal
        , nominalDistance :: NominalDistance
        , minimumDistance :: MinimumDistance
        , reachMax :: ReachToggle PilotDistance
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype SumOfDistance = SumOfDistance Double
    deriving (Eq, Ord)

newtype NominalDistanceArea = NominalDistanceArea Double
    deriving (Eq, Ord)
    deriving newtype (Show, FromJSON)

showNominalDistanceArea :: NominalDistanceArea -> T.Text
showNominalDistanceArea (NominalDistanceArea x) =
    T.pack $ printf "%.3f km" x

showNominalDistanceAreaDiff :: NominalDistanceArea -> NominalDistanceArea -> T.Text
showNominalDistanceAreaDiff (NominalDistanceArea expected) (NominalDistanceArea actual)
    | f actual == f expected = "="
    | otherwise = T.pack . f $ actual - expected
    where
        f = printf "%+.3f"

newtype NominalGoal = NominalGoal Double
    deriving (Eq, Ord)
    deriving newtype (Show, FromJSON)

showNominalGoal :: NominalGoal -> T.Text
showNominalGoal (NominalGoal x) =
    T.pack $ printf "%.2f" x

showNominalGoalDiff :: NominalGoal -> NominalGoal -> T.Text
showNominalGoalDiff (NominalGoal expected) (NominalGoal actual)
    | f actual == f expected = "="
    | otherwise = T.pack . f $ actual - expected
    where
        f = printf "%+.2f"

newtype NominalDistance = NominalDistance Double
    deriving (Eq, Ord)

newtype MinimumDistance = MinimumDistance Double
    deriving (Eq, Ord)

instance Show SumOfDistance where
    show (SumOfDistance x) = show x ++ " km"

showSumOfDistance :: SumOfDistance -> T.Text
showSumOfDistance (SumOfDistance x) =
    T.pack $ printf "%.3f km" x

showSumOfDistanceDiff :: SumOfDistance -> SumOfDistance -> T.Text
showSumOfDistanceDiff (SumOfDistance expected) (SumOfDistance actual)
    | f actual == f expected = "="
    | otherwise = T.pack . f $ actual - expected
    where
        f = printf "%+.3f"

instance Show NominalDistance where
    show (NominalDistance x) = show x ++ " km"

showNominalDistance :: NominalDistance -> T.Text
showNominalDistance (NominalDistance x) =
    T.pack $ printf "%.3f km" x

showNominalDistanceDiff :: NominalDistance -> NominalDistance -> T.Text
showNominalDistanceDiff (NominalDistance expected) (NominalDistance actual)
    | f actual == f expected = "="
    | otherwise = T.pack . f $ actual - expected
    where
        f = printf "%+.3f"

instance Show MinimumDistance where
    show (MinimumDistance x) = show x ++ " km"

showMinimumDistance :: MinimumDistance -> T.Text
showMinimumDistance (MinimumDistance x) =
    T.pack $ printf "%.3f km" x

showMinimumDistanceDiff :: MinimumDistance -> MinimumDistance -> T.Text
showMinimumDistanceDiff (MinimumDistance expected) (MinimumDistance actual)
    | f actual == f expected = "="
    | otherwise = T.pack . f $ actual - expected
    where
        f = printf "%+.3f"

instance FromJSON SumOfDistance where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : 'k' : ' ' : xs -> return . SumOfDistance . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance FromJSON NominalDistance where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : 'k' : ' ' : xs -> return . NominalDistance . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance FromJSON MinimumDistance where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : 'k' : ' ' : xs -> return . MinimumDistance . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

data TimeValidityWorking =
    TimeValidityWorking
        { ssBestTime :: Maybe BestTime
        , gsBestTime :: Maybe BestTime
        , nominalTime :: NominalTime
        , nominalDistance :: NominalDistance
        , reachMax :: ReachToggle PilotDistance
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

newtype BestTime = BestTime Double
    deriving (Eq, Ord)

newtype NominalTime = NominalTime Double
    deriving (Eq, Ord)

instance Show BestTime where
    show (BestTime x) = T.unpack $ showHmsForHours x

showBestTime :: Maybe BestTime -> T.Text
showBestTime Nothing = "-"
showBestTime (Just (BestTime x)) = showHmsForHours x

showBestTimeDiff :: Maybe BestTime -> Maybe BestTime -> T.Text
showBestTimeDiff Nothing _ = "-"
showBestTimeDiff _ Nothing = "-"
showBestTimeDiff e@(Just (BestTime expected)) a@(Just (BestTime actual))
    | showBestTime a == showBestTime e = "="
    | otherwise = showNominalTDiff . realToFrac $ actual - expected

instance Show NominalTime where
    show (NominalTime x) = show x ++ " h"

showNominalTime :: NominalTime -> T.Text
showNominalTime (NominalTime x) = showHmsForHours x

showNominalTimeDiff :: NominalTime -> NominalTime -> T.Text
showNominalTimeDiff e@(NominalTime expected) a@(NominalTime actual)
    | showNominalTime a == showNominalTime e = "="
    | otherwise = showNominalTDiff . realToFrac $ actual - expected

instance FromJSON BestTime where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'h' : ' ' : xs -> return . BestTime . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance FromJSON NominalTime where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'h' : ' ' : xs -> return . NominalTime . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

data ReachStats =
    ReachStats
        { max :: PilotDistance
        , mean :: PilotDistance
        , stdDev :: PilotDistance
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

data StopValidityWorking =
    StopValidityWorking
        { pilotsAtEss :: PilotsAtEss
        , landed :: PilotsLanded
        , stillFlying :: PilotsFlying
        , flying :: PilotsFlying
        , launchToEssDistance :: Maybe LaunchToEss
        , reachStats :: ReachToggle (Maybe ReachStats)
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

newtype PilotsLanded = PilotsLanded Integer
    deriving (Eq, Ord)
    deriving newtype (Show, FromJSON)

newtype PilotsAtEss = PilotsAtEss Integer
    deriving (Eq, Ord)
    deriving newtype (Show, FromJSON)

showPilotsLandedDiff :: PilotsLanded -> PilotsLanded -> T.Text
showPilotsLandedDiff (PilotsLanded expected) (PilotsLanded actual)
    | f actual == f expected = "="
    | otherwise = T.pack . f $ actual - expected
    where
        f = printf "%+d"

showPilotsAtEssDiff :: PilotsAtEss -> PilotsAtEss -> T.Text
showPilotsAtEssDiff (PilotsAtEss expected) (PilotsAtEss actual)
    | f actual == f expected = "="
    | otherwise = T.pack . f $ actual - expected
    where
        f = printf "%+d"

newtype LaunchToEss = LaunchToEss Double
    deriving (Eq, Ord, Show, Generic)

instance FromJSON LaunchToEss where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : 'k' : ' ' : xs -> return . LaunchToEss . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

showLaunchToEss :: LaunchToEss -> T.Text
showLaunchToEss (LaunchToEss d) =
    T.pack . printf "%.3f km" $ d

showLaunchToEssDiff :: LaunchToEss -> LaunchToEss -> T.Text
showLaunchToEssDiff (LaunchToEss expected) (LaunchToEss actual)
    | f actual == f expected = "="
    | otherwise = T.pack . f $ actual - expected
    where
        f = printf "%+.3f"
