{-# LANGUAGE DuplicateRecordFields #-}

module WireTypes.Cross
    ( FlyingSection
    , TrackFlyingSection(..)
    , TrackScoredSection(..)
    , Fix(..)
    , InterpolatedFix(..)
    , ZoneTag(..)
    , ZoneCross(..)
    , TrackCross(..)
    ) where

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))
import WireTypes.Zone (RawLat(..), RawLng(..))
import WireTypes.Point (StartGate(..))

type FlyingSection a = Maybe (a, a)

data TrackFlyingSection =
    TrackFlyingSection
        { loggedFixes :: Maybe Int
        , flyingFixes :: FlyingSection Int
        , flyingTimes :: FlyingSection UTCTime
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data TrackScoredSection =
    TrackScoredSection
        { scoredFixes :: FlyingSection Int
        , scoredTimes :: FlyingSection UTCTime
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data Fix =
    Fix { fix :: Int
        , time :: UTCTime
        , lat :: RawLat
        , lng :: RawLng
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data InterpolatedFix =
    InterpolatedFix
        { fixFrac :: Double
        , time :: UTCTime
        , lat :: RawLat
        , lng :: RawLng
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data ZoneTag =
    ZoneTag
        { inter :: InterpolatedFix
        , cross :: ZoneCross
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data ZoneCross =
    ZoneCross
        { crossingPair :: [Fix]
        , inZone :: [Bool]
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data TrackCross =
    TrackCross
        { zonesCrossSelected :: [Maybe ZoneCross]
        , zonesCrossNominees :: [[Maybe ZoneCross]]
        , zonesCrossExcluded :: [[Maybe ZoneCross]]
        , startSelected :: Maybe (StartGate, ZoneCross)
        , startNominees :: [(StartGate, [ZoneCross])]
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)
