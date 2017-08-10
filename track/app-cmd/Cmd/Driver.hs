{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cmd.Driver (driverMain) where

import Control.Monad (mapM_)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (FilePath, takeFileName, takeDirectory)

import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..), Reckon(..))
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)

import Data.Flight.Comp
    ( CompSettings(..)
    , Pilot(..)
    )
import Data.Flight.TrackLog
    ( PilotTrackFixes(..)
    , TrackFileFail(..)
    , Task
    , goalPilotTracks
    , filterPilots
    , filterTasks
    , makeAbsolute
    )

driverMain :: IO ()
driverMain = withCmdArgs drive

drive :: CmdOptions -> IO ()
drive CmdOptions{..} = do
    dfe <- doesFileExist file
    if dfe then
        go file
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- find always (fileType ==? RegularFile &&? extension ==? ".comp.yaml") dir
            mapM_ go files
        else
            putStrLn "Couldn't find any flight score competition yaml input files."
    where
        go yamlCompPath = do
            putStrLn $ takeFileName yamlCompPath

            case reckon of
                Goal -> do
                    made <- runExceptT $ readMadeGoal
                                            yamlCompPath
                                            task
                                            (Pilot <$> pilot)
                    case made of
                        Left msg -> print msg
                        Right tracks -> print tracks

                x ->
                    putStrLn $ "TODO: Handle other reckon of " ++ show x

readSettings :: FilePath -> ExceptT String IO CompSettings
readSettings compYamlPath = do
    contents <- lift $ BS.readFile compYamlPath
    ExceptT . return $ decodeEither contents

readMadeGoal :: FilePath
             -> [Task]
             -> [Pilot]
             -> ExceptT
                 String
                 IO
                 [[ Either
                     (Pilot, TrackFileFail)
                     (Pilot, PilotTrackFixes)
                 ]]
readMadeGoal compYamlPath tasks selectPilots = do
    (CompSettings {pilots, taskFolders}) <- readSettings compYamlPath

    let dir = takeDirectory compYamlPath
    let ys = filterPilots selectPilots $ filterTasks tasks pilots
    let fs = (makeAbsolute dir) <$> taskFolders
    let zs = zipWith (\f y -> f <$> y) fs ys

    lift $ goalPilotTracks zs
