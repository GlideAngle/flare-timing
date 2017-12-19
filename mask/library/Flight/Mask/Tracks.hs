module Flight.Mask.Tracks (checkTracks) where

import Control.Monad.Except (ExceptT(..), lift)
import System.FilePath (takeDirectory)

import Flight.Kml (MarkedFixes)
import Flight.Comp 
    ( CompSettings(..)
    , Pilot(..)
    , PilotTrackLogFile(..)
    , TrackFileFail(..)
    , CompInputFile(..)
    )
import Flight.TrackLog as Log
    (IxTask(..), pilotTracks, filterPilots, filterTasks, makeAbsolute)
import Flight.Units ()
import Flight.Scribe (readComp)

settingsLogs :: CompInputFile
             -> [IxTask]
             -> [Pilot]
             -> ExceptT String IO (CompSettings, [[PilotTrackLogFile]])
settingsLogs compFile@(CompInputFile path) tasks selectPilots = do
    settings <- readComp compFile
    ExceptT . return $ go settings
    where
        go s@CompSettings{pilots, taskFolders} =
            Right (s, zs)
            where
                dir = takeDirectory path
                ys = Log.filterPilots selectPilots $ Log.filterTasks tasks pilots
                fs = Log.makeAbsolute dir <$> taskFolders
                zs = zipWith (<$>) fs ys

checkTracks :: forall a. (CompSettings -> (IxTask -> MarkedFixes -> a))
            -> CompInputFile
            -> [IxTask]
            -> [Pilot]
            -> ExceptT
                String
                IO
                [[ Either
                   (Pilot, TrackFileFail)
                   (Pilot, a)
                ]]
checkTracks f compFile tasks selectPilots = do
    (settings, xs) <- settingsLogs compFile tasks selectPilots
    lift $ Log.pilotTracks (f settings) xs
