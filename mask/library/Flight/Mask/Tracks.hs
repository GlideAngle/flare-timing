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
import System.FilePath (FilePath, takeDirectory)

import qualified Flight.Kml as Kml (MarkedFixes(..))
import qualified Flight.Comp as Cmp
    ( CompSettings(..)
    , Pilot(..)
    , PilotTrackLogFile(..)
    )
import Flight.TrackLog as Log
    ( TrackFileFail(..)
    , IxTask(..)
    , pilotTracks
    , filterPilots
    , filterTasks
    , makeAbsolute
    )
import Flight.Units ()
import Flight.Mask.Settings (readCompSettings)

settingsLogs :: FilePath
             -> [IxTask]
             -> [Cmp.Pilot]
             -> ExceptT String IO (Cmp.CompSettings, [[Cmp.PilotTrackLogFile]])
settingsLogs compYamlPath tasks selectPilots = do
    settings <- readCompSettings compYamlPath
    ExceptT . return $ go settings
    where
        go s@Cmp.CompSettings{pilots, taskFolders} =
            Right (s, zs)
            where
                dir = takeDirectory compYamlPath
                ys = Log.filterPilots selectPilots $ Log.filterTasks tasks pilots
                fs = Log.makeAbsolute dir <$> taskFolders
                zs = zipWith (<$>) fs ys

checkTracks :: forall a. (Cmp.CompSettings -> (IxTask -> Kml.MarkedFixes -> a))
            -> FilePath
            -> [IxTask]
            -> [Cmp.Pilot]
            -> ExceptT
                String
                IO
                [[ Either
                   (Cmp.Pilot, TrackFileFail)
                   (Cmp.Pilot, a)
                ]]
checkTracks f compYamlPath tasks selectPilots = do
    (settings, xs) <- settingsLogs compYamlPath tasks selectPilots
    lift $ Log.pilotTracks (f settings) xs
