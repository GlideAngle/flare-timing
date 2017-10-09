{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cmd.Options (CmdOptions(..), Reckon(..)) where

import System.Console.CmdArgs.Implicit (Default(..), Data, Typeable)
import Flight.TaskTrack (TaskDistanceMeasure(..))

deriving instance Data TaskDistanceMeasure

instance Default TaskDistanceMeasure where
    def = TaskDistanceByAllMethods

-- | Options passed in on the command line.
data CmdOptions
    = CmdOptions { dir :: FilePath
                 -- ^ Picking all competition in this directory.
                 , file :: FilePath
                 -- ^ Picking the competition in this file.
                 , task :: [Int]
                 -- ^ Include only these tasks.
                 , pilot :: [String]
                 -- ^ Look only at these pilots
                 , reckon :: Reckon
                 -- ^ Do the specified reckonings only.
                 , measure :: TaskDistanceMeasure
                 -- ^ Use the given measure(s).
                 }
                 deriving (Data, Typeable, Show)

-- | The reckonings of task track logs.
data Reckon
    = All
    -- ^ All the other reckonings combined.
    | Launch
    -- ^ Did the pilot launch?
    | Goal
    -- ^ Was goal made?
    | Zones
    -- ^ What zones were made?
    | GoalDistance
    -- ^ What is the distance to goal?
    | FlownDistance
    -- ^ What is the distance flown as task distance minus distance to goal?
    | Time 
    -- ^ What was the time to fly the speed section?
    | Lead
    -- ^ What is the leading coefficient?
    deriving (Data, Typeable, Eq, Show)

instance Default Reckon where
    def = All
