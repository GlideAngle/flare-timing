{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Flight.Mask.Tracks (checkTracks) where

import Control.Monad.Except (ExceptT(..), lift)
import System.FilePath (takeDirectory)

import qualified Flight.Kml as Kml (MarkedFixes(..))
import Flight.Comp 
    ( CompSettings(..)
    , Pilot(..)
    , PilotTrackLogFile(..)
    , TrackFileFail(..)
    , CompInputFile(..)
    )
import Flight.TrackLog as Log
    ( IxTask(..)
    , pilotTracks
    , filterPilots
    , filterTasks
    , makeAbsolute
    )
import Flight.Units ()
import Flight.Mask.Settings (readCompSettings)

settingsLogs :: CompInputFile
             -> [IxTask]
             -> [Pilot]
             -> ExceptT String IO (CompSettings, [[PilotTrackLogFile]])
settingsLogs (CompInputFile path) tasks selectPilots = do
    settings <- readCompSettings path
    ExceptT . return $ go settings
    where
        go s@CompSettings{pilots, taskFolders} =
            Right (s, zs)
            where
                dir = takeDirectory path
                ys = Log.filterPilots selectPilots $ Log.filterTasks tasks pilots
                fs = Log.makeAbsolute dir <$> taskFolders
                zs = zipWith (<$>) fs ys

checkTracks :: forall a. (CompSettings -> (IxTask -> Kml.MarkedFixes -> a))
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
