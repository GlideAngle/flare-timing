{-# LANGUAGE DeriveDataTypeable #-}

module Cmd.Options (CmdOptions(..), Reckon(..)) where

import System.Console.CmdArgs.Implicit (Default(..), Data, Typeable)

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
                 }
                 deriving (Data, Typeable, Show)

-- | The reckonings of task track logs.
data Reckon
    = Fixes
    -- ^ How many fixes?
    | Launch
    -- ^ Did the pilot launch?
    | Started 
    -- ^ Did the pilot start the speed section?
    | Goal
    -- ^ Was goal made?
    | Zones
    -- ^ What zones were made?
    | GoalDistance
    -- ^ What is the distance to goal?
    | Time 
    -- ^ What was the time to fly the speed section?
    | Lead
    -- ^ What is the leading coefficient?
    deriving (Data, Typeable, Eq, Show)

instance Default Reckon where
    def = Goal
