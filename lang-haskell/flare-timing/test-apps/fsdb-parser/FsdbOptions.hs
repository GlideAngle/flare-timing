{-# LANGUAGE DeriveDataTypeable #-}

module FsdbOptions (FsdbOptions(..), Detail(..), mkOptions) where

import Text.RawString.QQ (r)
import System.Console.CmdArgs.Implicit
    ( Data
    , Typeable
    , Default(def)
    , summary
    , program
    , groupname
    , typ
    , help
    , (&=)
    )

-- | Options passed in on the command line.
data FsdbOptions
    = FsdbOptions
        { file :: FilePath
        -- ^ Picking the competition in this file.
        , detail :: [Detail]
        -- ^ What details to show. If none supplied then all are
        -- shown.
        }
    deriving (Show, Data, Typeable)

-- | Extract these details from the flight database XML file.
data Detail
    = Comp
    -- ^ The competition, its name, location and dates.
    | Nominals
    -- ^ The nominal values that are set before competition begins that help
    -- determine baselines for valid tasks.
    | ScoreBack 
    -- ^ The score back time set for the competition.
    | Pilots
    -- ^ The pilots flying in each task.
    | Tasks
    -- ^ Each task with its control zones.
    | TaskFolders
    -- ^ The folder for each task's track logs.
    | PilotTracks
    -- ^ The location of flown track logs, for each pilot flying each task.
    deriving (Data, Eq, Show)

description :: String -> String
description programName =
    intro ++ programName ++ about
    where
        intro =
            [r|
Commission Internationale de Vol Libre (CIVL - Hang Gliding and Paragliding
Commission) is an Air Sport Commission (ASC) of the Fédération Internationale
Aéronautique (FAI). CIVL produce FS, the official software for scoring hang
gliding and paragliding competitions. FSDB is the database of FS, an XML format
for inputs, working and outputs of scoring.
|]

        about =
            [r| is a parser for a subset of the FSDB, just enough to cover the inputs of scoring.
|]

mkOptions :: String -> FsdbOptions
mkOptions programName =
    FsdbOptions
        { file = def
        &= help "With this one FSDB file"
        &= groupname "Source"

        , detail = def
        &= help "Focus on these details"
        &= typ "tasks | nominals"
        &= groupname "Filter"
        }
        &= summary (description programName)
        &= program programName
