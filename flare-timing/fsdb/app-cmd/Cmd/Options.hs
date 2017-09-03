{-# LANGUAGE DeriveDataTypeable #-}

module Cmd.Options (CmdOptions(..), Detail(..)) where

import System.Console.CmdArgs.Implicit (Data)

-- | Options passed in on the command line.
data CmdOptions
    = CmdOptions { dir :: FilePath
                 -- ^ Picking all competition in this directory.
                 , file :: FilePath
                 -- ^ Picking the competition in this file.
                 , detail :: [Detail]
                 -- ^ What details to show. If none supplied then all are
                 -- shown.
                 }
    deriving Show

-- | Extract these details from the flight database XML file.
data Detail
    = Comp
    -- ^ The competition, its name, location and dates.
    | Nominals
    -- ^ The nominal values that are set before competition begins that help
    -- determine baselines for valid tasks.
    | Pilots
    -- ^ The pilots flying in each task.
    | Tasks
    -- ^ Each task with its control zones.
    | TaskFolders
    -- ^ The folder for each task's track logs.
    | PilotTracks
    -- ^ The location of flown track logs, for each pilot flying each task.
    deriving (Data, Eq, Show)
