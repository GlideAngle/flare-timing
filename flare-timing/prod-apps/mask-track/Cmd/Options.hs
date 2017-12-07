{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
                 , measure :: TaskDistanceMeasure
                 -- ^ Use the given measure(s).
                 }
                 deriving (Data, Typeable, Show)

description :: String
description = [r|
By masking the track logs with the zones, works out;
* if the pilot launched
* if they made goal then
    * how long the pilot took to reach goal
* if they landed out then
    * how far they got along the course
    * how far yet to reach goal

Where 'c' is the comp name, 'p' is the pilot name, '.' is the folder with
competition inputs and k is a folder path specified in the inputs for
tracklogs, one per task;
    Reads  ./c.comp-input.yaml
    Reads  ./k/p.kml
    Reads  ./c.tag-zone.yaml
    Writes ./flare-timing/discard-further/task-n/p.csv
    Writes ./c.mask-track.yaml

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

        , measure = def
        &= help "Which way to measure task distances, taskdistancebyallmethods|taskdistancebypoints|taskdistancebyedges"
        &= typ "METHOD"
        &= groupname "Filter"
        }
        &= summary description
        &= program programName
