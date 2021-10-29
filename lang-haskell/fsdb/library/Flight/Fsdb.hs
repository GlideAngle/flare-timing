{-|
Module      : Flight.Fsdb
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Provides parsing the fsdb format for competitors, tasks and results.
-}
module Flight.Fsdb
    ( cleanComp
    , trimComp
    , parseComp
    , parseNominal
    , parseTweak
    , parseScoreBack
    , parseTasks
    , parseTaskPilotGroups
    , parseTaskPilotPenaltiesAuto
    , parseTaskPilotPenalties
    , parsePilots
    , parseTracks
    , parseTaskFolders
    , parseAltScores
    , parseAltRoutes
    , parseAltArrivals
    , parseAltLandouts
    ) where

import Flight.Fsdb.Clean
import Flight.Fsdb.Trim
import Flight.Fsdb.Comp
import Flight.Fsdb.Nominal
import Flight.Fsdb.Tweak
import Flight.Fsdb.Stopped
import Flight.Fsdb.Task
import Flight.Fsdb.TaskArrival
import Flight.Fsdb.TaskEffort
import Flight.Fsdb.TaskRoute
import Flight.Fsdb.TaskScore
import Flight.Fsdb.Pilot
