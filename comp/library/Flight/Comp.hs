{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Data.Flight.Comp
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Data for competitions, competitors and tasks.
-}
module Flight.Comp
    ( -- * Competition
      CompSettings(..)
    , Comp(..)
    , Nominal(..)
    , UtcOffset(..)
    -- * Task
    , Task(..)
    , IxTask(..)
    , SpeedSection
    , StartGate(..)
    , OpenClose(..)
    , showTask
    -- * Pilot and their track logs.
    , Pilot(..)
    , PilotTrackLogFile(..)
    , TrackLogFile(..)
    , TrackFileFail(..)
    , TaskFolder(..)
    , FlyingSection
    -- * Comp paths
    , module Flight.Path
    ) where

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.List (intercalate)
import Data.String (IsString())

import Flight.Zone.Raw (RawZone, showZone)
import Flight.Field (FieldOrdering(..))
import Flight.Pilot
import Flight.Path

-- | 1-based indices of a task in a competition.
newtype IxTask = IxTask Int deriving (Eq, Show)

-- | A 1-based index into the list of control zones marking the speed section.
type SpeedSection = Maybe (Integer, Integer)

-- | A pair into the list of fixes marking those deemed logged while flying.
-- These could be indices, seconds offsets or UTC times.
type FlyingSection a = Maybe (a, a)

newtype StartGate = StartGate UTCTime
    deriving (Eq, Ord, Show, Generic)

instance ToJSON StartGate
instance FromJSON StartGate

newtype UtcOffset =
    UtcOffset { timeZoneMinutes :: Int }
    deriving (Eq, Ord, Show, Generic)

instance ToJSON UtcOffset
instance FromJSON UtcOffset

data OpenClose =
    OpenClose { open :: UTCTime 
              , close :: UTCTime
              }
              deriving (Eq, Ord, Show, Generic)

instance ToJSON OpenClose
instance FromJSON OpenClose

data CompSettings =
    CompSettings { comp :: Comp
                 , nominal :: Nominal
                 , tasks :: [Task]
                 , taskFolders :: [TaskFolder]
                 , pilots :: [[PilotTrackLogFile]]
                 }
                 deriving (Eq, Ord, Show, Generic)

instance ToJSON CompSettings
instance FromJSON CompSettings

data Comp =
    Comp { civilId :: String
         , compName :: String 
         , location :: String 
         , from :: String 
         , to :: String 
         , utcOffset :: UtcOffset
         }
         deriving (Eq, Ord, Show, Generic)

instance ToJSON Comp
instance FromJSON Comp

data Nominal =
    Nominal { distance :: String
            , time :: String 
            , goal :: String 
            }
            deriving (Eq, Ord, Show, Generic)

instance ToJSON Nominal
instance FromJSON Nominal

data Task =
    Task { taskName :: String
         , zones :: [RawZone]
         , speedSection :: SpeedSection
         , zoneTimes :: [OpenClose]
         , startGates :: [StartGate]
         } deriving (Eq, Ord, Show, Generic)

instance ToJSON Task
instance FromJSON Task

showTask :: Task -> String
showTask Task {taskName, zones, speedSection, zoneTimes, startGates} =
    unwords [ "Task '" ++ taskName ++ "'"
            , ", zones "
            , intercalate ", " $ showZone <$> zones
            , ", speed section "
            , show speedSection
            , ", zone times"
            , show zoneTimes 
            , ", start gates "
            , intercalate ", " $ show <$> startGates 
            ]

instance FieldOrdering CompSettings where
    fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        -- CompSettings fields
        ("comp", _) -> LT
        ("nominal", "comp") -> GT
        ("nominal", _) -> LT
        ("tasks", "taskFolders") -> LT
        ("tasks", "pilots") -> LT
        ("tasks", _) -> GT
        ("taskFolders", "pilots") -> LT
        ("taskFolders", _) -> GT
        ("pilots", _) -> GT
        -- Comp fields
        ("compName", _) -> LT
        ("location", "compName") -> GT
        ("location", _) -> LT
        ("from", "to") -> LT
        ("civilId", "utcOffset") -> LT
        ("civilId", _) -> GT
        ("utcOffset", _) -> GT
        -- Task fields
        ("taskName", _) -> LT
        ("zones", "taskName") -> GT
        ("zones", _) -> LT
        ("speedSection", "zoneTimes") -> LT
        ("speedSection", "startGates") -> LT
        ("speedSection", _) -> GT
        ("zoneTimes", "startGates") -> LT
        ("zoneTimes", _) -> GT
        ("startGates", _) -> GT
        ("open", _) -> LT
        ("close", _) -> GT
        -- Turnpoint fields
        ("zoneName", _) -> LT
        ("lat", "zoneName") -> GT
        ("lat", _) -> LT
        ("lng", "zoneName") -> GT
        ("lng", "lat") -> GT
        ("lng", _) -> LT
        ("radius", _) -> GT
        _ -> compare a b

