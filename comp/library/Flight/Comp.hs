{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

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
    ) where

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.List (intercalate)

import Flight.Zone.Raw (RawZone, showZone)
import Flight.Pilot

-- | A 1-based index into the list of control zones marking the speed section.
type SpeedSection = Maybe (Integer, Integer)

newtype StartGate = StartGate UTCTime deriving (Show, Eq, Generic)

instance ToJSON StartGate
instance FromJSON StartGate

newtype UtcOffset =
    UtcOffset { timeZoneMinutes :: Int } deriving (Show, Eq, Generic)

instance ToJSON UtcOffset
instance FromJSON UtcOffset

data OpenClose =
    OpenClose { open :: UTCTime 
              , close :: UTCTime
              } deriving (Show, Eq, Generic)

instance ToJSON OpenClose
instance FromJSON OpenClose

data CompSettings =
    CompSettings { comp :: Comp
                 , nominal :: Nominal
                 , tasks :: [Task]
                 , taskFolders :: [TaskFolder]
                 , pilots :: [[PilotTrackLogFile]]
                 } deriving (Show, Generic)

instance ToJSON CompSettings
instance FromJSON CompSettings

data Comp = Comp { civilId :: String
                 , compName :: String 
                 , location :: String 
                 , from :: String 
                 , to :: String 
                 , utcOffset :: UtcOffset
                 } deriving (Show, Generic)

instance ToJSON Comp
instance FromJSON Comp

data Nominal = Nominal { distance :: String
                       , time :: String 
                       , goal :: String 
                       } deriving (Show, Generic)

instance ToJSON Nominal
instance FromJSON Nominal

data Task =
    Task { taskName :: String
         , zones :: [RawZone]
         , speedSection :: SpeedSection
         , zoneTimes :: [OpenClose]
         , startGates :: [StartGate]
         } deriving (Eq, Show, Generic)

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
