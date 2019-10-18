{-# LANGUAGE DuplicateRecordFields #-}

module WireTypes.Validity
    ( Validity(..)
    , TaskValidity(..)
    , LaunchValidity(..)
    , DistanceValidity(..)
    , TimeValidity(..)
    , StopValidity(..)
    -- * Showing Validities
    , showLaunchValidity, showLaunchValidityDiff
    , showDistanceValidity, showDistanceValidityDiff
    , showTimeValidity, showTimeValidityDiff
    , showTaskValidity, showTaskValidityDiff
    , showStopValidity, showStopValidityDiff
    ) where

import Text.Printf (printf)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))
import qualified Data.Text as T (Text, pack)

newtype TaskValidity = TaskValidity Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype LaunchValidity = LaunchValidity Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype DistanceValidity = DistanceValidity Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype TimeValidity = TimeValidity Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

newtype StopValidity = StopValidity Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

pprVy :: Double -> String
pprVy = printf "%.3f"

showLaunchValidity :: LaunchValidity -> T.Text
showLaunchValidity (LaunchValidity v) = T.pack . pprVy $ v

showLaunchValidityDiff :: LaunchValidity -> LaunchValidity -> T.Text
showLaunchValidityDiff (LaunchValidity expected) (LaunchValidity actual)
    | f actual == f expected = "="
    | otherwise = T.pack . f $ actual - expected
    where
        f = printf "%+.3f"

showDistanceValidity :: DistanceValidity -> T.Text
showDistanceValidity (DistanceValidity v) = T.pack . pprVy $ v

showDistanceValidityDiff :: DistanceValidity -> DistanceValidity -> T.Text
showDistanceValidityDiff (DistanceValidity expected) (DistanceValidity actual)
    | f actual == f expected = "="
    | otherwise = T.pack . f $ actual - expected
    where
        f = printf "%+.3f"

showTimeValidity :: TimeValidity -> T.Text
showTimeValidity (TimeValidity v) = T.pack . pprVy $ v

showTimeValidityDiff :: TimeValidity -> TimeValidity -> T.Text
showTimeValidityDiff (TimeValidity expected) (TimeValidity actual)
    | f actual == f expected = "="
    | otherwise = T.pack . f $ actual - expected
    where
        f = printf "%+.3f"

showTaskValidity :: TaskValidity -> T.Text
showTaskValidity (TaskValidity v) = T.pack . pprVy $ v

showTaskValidityDiff :: TaskValidity -> TaskValidity -> T.Text
showTaskValidityDiff (TaskValidity expected) (TaskValidity actual)
    | f actual == f expected = "="
    | otherwise = T.pack . f $ actual - expected
    where
        f = printf "%+.3f"

showStopValidity :: Maybe StopValidity -> T.Text
showStopValidity Nothing = "-"
showStopValidity (Just (StopValidity v)) = T.pack . pprVy $ v

showStopValidityDiff :: Maybe StopValidity -> Maybe StopValidity -> T.Text
showStopValidityDiff Nothing (Just (StopValidity 1)) = "="
showStopValidityDiff (Just (StopValidity 1)) Nothing = "="
showStopValidityDiff Nothing _ = "-"
showStopValidityDiff _ Nothing = "-"
showStopValidityDiff (Just (StopValidity expected)) (Just (StopValidity actual))
    | f actual == f expected = "="
    | otherwise = T.pack . f $ actual - expected
    where
        f = printf "%+.3f"

data Validity =
    Validity
        { task :: TaskValidity
        , launch :: LaunchValidity
        , distance :: DistanceValidity
        , time :: TimeValidity
        , stop :: Maybe StopValidity
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)
