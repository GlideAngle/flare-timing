{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cmd.Driver (driverMain) where

import Control.Monad (mapM_, when)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (takeFileName)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)

import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..), Detail(..))
import Data.Flight.Fsdb
    ( parseComp
    , parseNominal
    , parseTasks
    , parsePilots
    , parseTracks
    , parseTaskFolders
    )
import Flight.Comp (Pilot(..), PilotTrackLogFile(..), showTask)

driverMain :: IO ()
driverMain = withCmdArgs drive

showTaskPilots :: [ (Int, [ Pilot ]) ] -> [ String ]
showTaskPilots [] = [ "No tasks." ]
showTaskPilots xs =
    (\(i, pilots) -> "Task #" ++ show i ++ " pilots: " ++ show pilots) <$> xs

showPilots :: [[ Pilot ]] -> String
showPilots [] = "No pilots."
showPilots (comp : tasks) =
    unlines $ ("Comp pilots: " ++ show comp) : showTaskPilots (zip [ 1 .. ] tasks) 

showTaskPilotTracks :: [ (Int, [ PilotTrackLogFile ]) ] -> [ String ]
showTaskPilotTracks [] = [ "No tasks." ]
showTaskPilotTracks xs =
    (\(i, pilotTracks) -> "Task #" ++ show i ++ " pilot tracks: " ++ show pilotTracks) <$> xs

showPilotTracks :: [[ PilotTrackLogFile ]] -> String
showPilotTracks [] = "No pilots."
showPilotTracks tasks =
    unlines $ showTaskPilotTracks (zip [ 1 .. ] tasks) 

drive :: CmdOptions -> IO ()
drive CmdOptions{..} = do
    dfe <- doesFileExist file
    if dfe then
        go file
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- find always (fileType ==? RegularFile &&? extension ==? ".fsdb") dir
            mapM_ go files
        else
            putStrLn "Couldn't find any flight score competition database input files."
    where
        go path = do
            putStrLn $ takeFileName path
            contents <- readFile path
            let contents' = dropWhile (/= '<') contents

            when (null detail || Comp `elem` detail) $ printComp contents'
            when (null detail || Nominals `elem` detail) $ printNominal contents'
            when (null detail || Pilots `elem` detail) $ printPilotNames contents'
            when (null detail || Tasks `elem` detail) $ printTasks contents'
            when (null detail || TaskFolders `elem` detail) $ printTaskFolders contents'
            when (null detail || PilotTracks `elem` detail) $ printPilotTracks contents'

printNominal :: String -> IO ()
printNominal contents = do
    nominal <- parseNominal contents
    case nominal of
         Left msg -> print msg
         Right nominal' -> print nominal'

printPilotNames :: String -> IO ()
printPilotNames contents = do
    pilots <- parsePilots contents
    case pilots of
         Left msg -> print msg
         Right pilots' -> putStr $ showPilots pilots'

printPilotTracks :: String -> IO ()
printPilotTracks contents = do
    pilotTracks <- parseTracks contents
    case pilotTracks of
         Left msg -> print msg
         Right pilotTracks' -> putStr $ showPilotTracks pilotTracks'

printTaskFolders :: String -> IO ()
printTaskFolders contents = do
    taskFolders <- parseTaskFolders contents
    case taskFolders of
         Left msg -> print msg
         Right taskFolders' -> print taskFolders'

printTasks :: String -> IO ()
printTasks contents = do
    tasks <- parseTasks contents
    case tasks of
         Left msg -> print msg
         Right tasks' -> print $ showTask <$> tasks'

printComp :: String -> IO ()
printComp contents = do
    comp <- parseComp contents
    case comp of
         Left msg -> print msg
         Right comp' -> print comp'
