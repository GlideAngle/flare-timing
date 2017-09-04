{-|
Module      : Data.Flight.Fsdb
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Provides parsing the fsdb format for competitors, tasks and results.
-}
module Data.Flight.Fsdb
    ( parseComp
    , parseNominal
    , parseTasks
    , parsePilots
    , parseTracks
    , parseTaskFolders
    ) where

import Data.Flight.Fsdb.Comp
import Data.Flight.Fsdb.Nominal
import Data.Flight.Fsdb.Task
import Data.Flight.Fsdb.Pilot
