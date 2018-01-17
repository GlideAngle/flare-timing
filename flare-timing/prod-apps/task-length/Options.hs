{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Options (CmdOptions(..), mkOptions) where

import Text.RawString.QQ (r)
import System.Console.CmdArgs.Implicit
    ( Data
    , Typeable
    , Default(def)
    , summary
    , program
    , groupname
    , typ
    , name
    , opt
    , explicit
    , help
    , (&=)
    )
import Flight.Route (TaskDistanceMeasure(..))

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
                 , measure :: TaskDistanceMeasure
                 -- ^ Use the given measure(s).
                 , noTaskWaypoints :: Bool
                 -- ^ Exclude task waypoints
                 }
                 deriving (Data, Typeable, Show)

description :: String
description = [r|
Works out the task length by following an optimal route.

Where 'c' is the comp name and '.' is the folder with competition inputs;
    Reads  ./c.comp-inputs.yaml
    Writes ./c.task-length.yaml
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

        , measure = def
        &= help "Which way to measure task distances, taskdistancebyallmethods|taskdistancebypoints|taskdistancebyedges"
        &= typ "METHOD"
        &= groupname "Filter"

        , noTaskWaypoints = def
        &= help "Exclude the task waypoints?"
        &= explicit
        &= name "no-task-waypoints"
        &= groupname "Filter"
        }
        &= summary description
        &= program programName
