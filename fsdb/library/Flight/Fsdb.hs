{-|
Module      : Flight.Fsdb
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Provides parsing the fsdb format for competitors, tasks and results.
-}
module Flight.Fsdb
    ( parseComp
    , parseNominal
    , parseTweak
    , parseScoreBack
    , parseTasks
    , parseTaskPilotGroups
    , parseTaskPilotPenalties
    , parsePilots
    , parseTracks
    , parseTaskFolders
    ) where

import Flight.Fsdb.Comp
import Flight.Fsdb.Nominal
import Flight.Fsdb.Tweak
import Flight.Fsdb.Stopped
import Flight.Fsdb.Task
import Flight.Fsdb.Pilot