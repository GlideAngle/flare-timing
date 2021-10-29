module Flight.Mask.Tracks (checkTracks, settingsLogs) where

import Control.DeepSeq
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)

import Flight.Kml (MarkedFixes)
import Flight.Comp
    ( IxTask(..)
    , CompTaskSettings(..)
    , Pilot(..)
    , PilotTrackLogFile(..)
    , TrackFileFail(..)
    , CompDir(..)
    , ScoringInputFiles
    , compFileToCompDir
    , mkCompTaskSettings
    )
import Flight.TrackLog as Log
    (pilotTracks, filterPilots, filterTasks, makeAbsolute)
import Flight.Units ()
import Flight.Scribe (readCompTracksQuietly)

settingsLogs
    :: (MonadThrow m, MonadIO m)
    => ScoringInputFiles
    -> [IxTask]
    -> [Pilot]
    -> m (CompTaskSettings k, [[PilotTrackLogFile]])
settingsLogs files@(compFile, _) tasks selectPilots = do
    (settings, (pilots, taskFolders)) <- readCompTracksQuietly files
    let CompDir dir = compFileToCompDir compFile
    let ys = Log.filterPilots selectPilots $ Log.filterTasks tasks pilots
    let fs = Log.makeAbsolute dir <$> taskFolders
    let zs = zipWith (<$>) fs ys
    return (uncurry mkCompTaskSettings $ settings, zs)

checkTracks
    :: (NFData a, MonadThrow m, MonadIO m)
    => (CompTaskSettings k -> (IxTask -> MarkedFixes -> a))
    -> ScoringInputFiles
    -> [IxTask]
    -> [Pilot]
    -> m
        [[ Either
           (Pilot, TrackFileFail)
           (Pilot, a)
        ]]
checkTracks f files tasks selectPilots = do
    (settings, xs) <- settingsLogs files tasks selectPilots
    liftIO $ Log.pilotTracks (f settings) xs
