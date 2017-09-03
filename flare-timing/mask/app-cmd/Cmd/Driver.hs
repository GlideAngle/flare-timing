{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Cmd.Driver (driverMain) where

import Control.Monad (mapM_)
import Control.Monad.Except (ExceptT(..), runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (FilePath, takeFileName)
import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..), Reckon(..))

import qualified Data.Flight.Comp as Cmp (CompSettings(..), Pilot(..))
import Data.Flight.TrackLog as Log (TrackFileFail(..), IxTask(..))
import Flight.Units ()
import Flight.Mask.Pilot
    ( countFixes
    , checkTracks
    , madeZones
    , madeSpeedZones
    , launched
    , madeGoal
    , started
    , distanceToGoal
    , distanceFlown
    , timeFlown
    )

driverMain :: IO ()
driverMain = withCmdArgs drive

drive :: CmdOptions -> IO ()
drive CmdOptions{..} = do
    dfe <- doesFileExist file
    if dfe then
        withFile file
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- find always (fileType ==? RegularFile &&? extension ==? ".comp.yaml") dir
            mapM_ withFile files
        else
            putStrLn "Couldn't find any flight score competition yaml input files."
    where
        withFile yamlCompPath = do
            putStrLn $ takeFileName yamlCompPath

            case reckon of
                Fixes -> go checkFixes
                Zones -> go checkZones
                SpeedZones -> go checkSpeedZones
                Launch -> go checkLaunched
                Started -> go checkStarted
                Goal -> go checkMadeGoal
                GoalDistance -> go checkDistanceToGoal
                FlownDistance -> go checkDistanceFlown
                Time -> go checkTimeToGoal
                x -> putStrLn $ "TODO: Handle other reckon of " ++ show x

            where
                go :: Show a => (FilePath
                   -> [IxTask]
                   -> [Cmp.Pilot]
                   -> ExceptT
                       String
                       IO
                       [[Either
                           (Cmp.Pilot, TrackFileFail)
                           (Cmp.Pilot, a)
                       ]])
                   -> IO ()
                go f = do
                    checks <-
                        runExceptT $
                            f
                                yamlCompPath
                                (IxTask <$> task)
                                (Cmp.Pilot <$> pilot)

                    print checks

                checkFixes =
                    checkTracks $ const (\_ xs -> countFixes xs)

                checkZones =
                    checkTracks $ \(Cmp.CompSettings {tasks}) -> madeZones tasks

                checkSpeedZones =
                    checkTracks $ \(Cmp.CompSettings {tasks}) -> madeSpeedZones tasks

                checkLaunched =
                    checkTracks $ \(Cmp.CompSettings {tasks}) -> launched tasks

                checkStarted =
                    checkTracks $ \(Cmp.CompSettings {tasks}) -> started tasks

                checkMadeGoal =
                    checkTracks $ \(Cmp.CompSettings {tasks}) -> madeGoal tasks

                checkDistanceToGoal =
                    checkTracks $ \(Cmp.CompSettings {tasks}) -> distanceToGoal tasks

                checkDistanceFlown =
                    checkTracks $ \(Cmp.CompSettings {tasks}) -> distanceFlown tasks

                checkTimeToGoal =
                    checkTracks $ \(Cmp.CompSettings {tasks}) -> timeFlown tasks

