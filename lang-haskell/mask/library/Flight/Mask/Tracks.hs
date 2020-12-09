module Flight.Mask.Tracks (checkTracks, settingsLogs) where

import Control.DeepSeq
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)

import Flight.Kml (MarkedFixes)
import Flight.Comp
    ( IxTask(..)
    , CompSettings(..)
    , Pilot(..)
    , PilotTrackLogFile(..)
    , TrackFileFail(..)
    , CompInputFile(..)
    , CompDir(..)
    , compFileToCompDir
    )
import Flight.TrackLog as Log
    (pilotTracks, filterPilots, filterTasks, makeAbsolute)
import Flight.Units ()
import Flight.Scribe (readComp)

settingsLogs
    :: (MonadThrow m, MonadIO m)
    => CompInputFile
    -> [IxTask]
    -> [Pilot]
    -> m (CompSettings k, [[PilotTrackLogFile]])
settingsLogs compFile tasks selectPilots = do
    settings <- readComp compFile
    go settings
    where
        go s@CompSettings{pilots, taskFolders} = do
            return (s, zs)
            where
                CompDir dir = compFileToCompDir compFile
                ys = Log.filterPilots selectPilots $ Log.filterTasks tasks pilots
                fs = Log.makeAbsolute dir <$> taskFolders
                zs = zipWith (<$>) fs ys

checkTracks
    :: (NFData a, MonadThrow m, MonadIO m)
    => (CompSettings k -> (IxTask -> MarkedFixes -> a))
    -> CompInputFile
    -> [IxTask]
    -> [Pilot]
    -> m
        [[ Either
           (Pilot, TrackFileFail)
           (Pilot, a)
        ]]
checkTracks f compFile tasks selectPilots = do
    (settings, xs) <- settingsLogs compFile tasks selectPilots
    liftIO $ Log.pilotTracks (f settings) xs
