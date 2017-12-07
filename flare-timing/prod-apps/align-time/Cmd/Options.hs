{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}

module Cmd.Options (CmdOptions(..), mkOptions) where

import Text.RawString.QQ (r)
import System.Console.CmdArgs.Implicit
    ( Data
    , Typeable
    , Default(def)
    , summary
    , program
    , groupname
    , typ
    , opt
    , help
    , (&=)
    )

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
                 }
                 deriving (Data, Typeable, Show)

description :: String
description = [r|
From competition inputs '.comp-input.yaml', relative track logs '.kml' and
tagged zones '.tag-zone.yaml', writes each fix to a single '.csv' per-pilot
per-task, grouped into a single folder per-task.

Using the tagged zones to find the first crossing of a pilot into the speed
section, three columns are added to the CSV;
    leg        The leg of the speed section of the task
    tick       The time in seconds from the first crossing
    distance   The distance to goal

Where 'c' is the comp name, 'p' is the pilot name, '.' is the folder with
competition inputs and k is a folder path specified in the inputs for
tracklogs, one per task;
    Reads  ./c.comp-input.yaml
    Reads  ./c.tag-zone.yaml
    Reads  ./k/p.kml
    Writes ./flare-timing/align-time/task-n/p.csv

If a list of tasks are supplied then those tasks alone are processed, otherwise
all tasks are processed. The same thing goes if a list of pilots is supplied or
not.
|]

mkOptions :: String -> CmdOptions
mkOptions programName =
    CmdOptions
        { dir = def
        &= help "Over all the competition *.comp.yaml files in this directory"
        &= groupname "Source"

        , file = def
        &= help "With this one competition *.comp.yaml file"
        &= groupname "Source"

        , task = def
        &= help "Which tasks?"
        &= typ "TASK NUMBER"
        &= opt "name"
        &= groupname "Filter"

        , pilot = def
        &= help "Which pilots?"
        &= typ "PILOT NAME"
        &= opt "name"
        &= groupname "Filter"
        }
        &= summary description
        &= program programName
