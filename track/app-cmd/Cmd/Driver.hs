{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cmd.Driver (driverMain) where

import Control.Lens ((^?), element)
import Control.Monad (mapM_)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (FilePath, takeFileName, takeDirectory)

import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..), Reckon(..))
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)

import qualified Data.Flight.Kml as K (Fix)
import qualified Data.Flight.Comp as C
    (CompSettings(..), Pilot(..), Task(..), PilotTrackLogFile(..))
import Data.Flight.TrackLog as T
    ( TrackFileFail(..)
    , Task
    , goalPilotTracks
    , filterPilots
    , filterTasks
    , makeAbsolute
    )

newtype PilotTrackFixes = PilotTrackFixes Int deriving Show

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
                Fixes -> do
                    made <- runExceptT $ checkFixes
                                            yamlCompPath
                                            task
                                            (C.Pilot <$> pilot)
                    case made of
                        Left msg -> print msg
                        Right tracks -> print tracks

                Launch -> do
                    made <- runExceptT $ checkLaunched
                                            yamlCompPath
                                            task
                                            (C.Pilot <$> pilot)
                    case made of
                        Left msg -> print msg
                        Right tracks -> print tracks

                x ->
                    putStrLn $ "TODO: Handle other reckon of " ++ show x

readSettings :: FilePath -> ExceptT String IO C.CompSettings
readSettings compYamlPath = do
    contents <- lift $ BS.readFile compYamlPath
    ExceptT . return $ decodeEither contents

settingsLogs :: FilePath
             -> [T.Task]
             -> [C.Pilot]
             -> ExceptT String IO (C.CompSettings, [[C.PilotTrackLogFile]])
settingsLogs compYamlPath tasks selectPilots = do
    settings <- readSettings compYamlPath
    ExceptT . return $ go settings
    where
        go s@(C.CompSettings {pilots, taskFolders}) =
            Right (s, zs)
            where
                dir = takeDirectory compYamlPath
                ys = T.filterPilots selectPilots $ T.filterTasks tasks pilots
                fs = (T.makeAbsolute dir) <$> taskFolders
                zs = zipWith (\f y -> f <$> y) fs ys

checkFixes :: FilePath
           -> [T.Task]
           -> [C.Pilot]
           -> ExceptT
               String
               IO
               [[ Either
                   (C.Pilot, TrackFileFail)
                   (C.Pilot, PilotTrackFixes)
               ]]
checkFixes compYamlPath tasks selectPilots = do
    (_, zs) <- settingsLogs compYamlPath tasks selectPilots
    lift $ T.goalPilotTracks (\_ xs -> countFixes xs) zs

checkLaunched :: FilePath
              -> [T.Task]
              -> [C.Pilot]
              -> ExceptT
                  String
                  IO
                  [[ Either
                      (C.Pilot, TrackFileFail)
                      (C.Pilot, Bool)
                  ]]
checkLaunched compYamlPath ts selectPilots = do
    (C.CompSettings {tasks}, zs) <- settingsLogs compYamlPath ts selectPilots
    lift $ T.goalPilotTracks (launched tasks) zs

countFixes :: [K.Fix] -> PilotTrackFixes
countFixes xs = PilotTrackFixes $ length xs

launched :: [C.Task] -> Int -> [K.Fix] -> Bool
launched tasks i _ =
    case tasks ^? element i of
        Nothing -> False
        Just _ -> True
