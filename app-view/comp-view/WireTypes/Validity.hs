{-# LANGUAGE DuplicateRecordFields #-}

module WireTypes.Validity
    ( Validity(..)
    , TaskValidity(..)
    , LaunchValidity(..)
    , DistanceValidity(..)
    , TimeValidity(..)
    -- * Showing Validities
    , showLaunchValidity
    , showDistanceValidity
    , showTimeValidity
    , showTaskValidity
    ) where

import Text.Printf (printf)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Text as T (Text, pack)

newtype TaskValidity = TaskValidity Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype LaunchValidity = LaunchValidity Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype DistanceValidity = DistanceValidity Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype TimeValidity = TimeValidity Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

pprVy :: Double -> String
pprVy = printf "%.3f"

showLaunchValidity :: LaunchValidity -> T.Text
showLaunchValidity (LaunchValidity v) = T.pack . pprVy $ v

showDistanceValidity :: DistanceValidity -> T.Text
showDistanceValidity (DistanceValidity v) = T.pack . pprVy $ v

showTimeValidity :: TimeValidity -> T.Text
showTimeValidity (TimeValidity v) = T.pack . pprVy $ v

showTaskValidity :: TaskValidity -> T.Text
showTaskValidity (TaskValidity v) = T.pack . pprVy $ v

data Validity =
    Validity 
        { task :: TaskValidity
        , launch :: LaunchValidity
        , distance :: DistanceValidity
        , time :: TimeValidity
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
