{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cmd.Driver (driverMain) where

import Data.Maybe (catMaybes)
import Control.Monad (mapM_)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (takeFileName)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)

import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..), Reckon(..))
import Data.Flight.Pilot
    ( Pilot(..), PilotTrackLogFile(..), parseTracks)

type Task = Int

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
            files <- find always (fileType ==? RegularFile &&? extension ==? ".fsdb") dir
            mapM_ go files
        else
            putStrLn "Couldn't find any flight score competition database input files."
    where
        go path = do
            putStrLn $ takeFileName path
            contents <- readFile path
            let contents' = dropWhile (/= '<') contents

            case reckon of
                Goal -> printMadeGoal task (Pilot <$> pilot) contents'
                x -> putStrLn $ "TODO: Handle other reckon of " ++ show x

showTaskPilotTracks :: [ (Int, [ PilotTrackLogFile ]) ] -> [ String ]
showTaskPilotTracks [] = [ "No tasks." ]
showTaskPilotTracks xs =
    (\(i, pilotTracks) -> "Task #" ++ show i ++ " pilot tracks: " ++ show pilotTracks) <$> xs

showPilotTracks :: [[ PilotTrackLogFile ]] -> String
showPilotTracks [] = "No pilots."
showPilotTracks tasks =
    unlines $ showTaskPilotTracks (zip [ 1 .. ] tasks) 

filterPilots :: [ Pilot ]
             -> [[ PilotTrackLogFile ]]
             ->[[ PilotTrackLogFile ]]

filterPilots [] xs = xs
filterPilots pilots xs =
    f <$> xs
    where
        f :: [ PilotTrackLogFile ] -> [ PilotTrackLogFile ]
        f ys =
            catMaybes
            $ (\x@(PilotTrackLogFile pilot _) ->
                if pilot `elem` pilots then Just x else Nothing)
            <$> ys

filterTasks :: [ Task ]
            -> [[ PilotTrackLogFile ]]
            ->[[ PilotTrackLogFile ]]

filterTasks [] xs = xs
filterTasks tasks xs =
    zipWith (\i ys ->
        if i `elem` tasks then ys else []) [ 1 .. ] xs

printMadeGoal :: [ Task ] -> [ Pilot ] -> String -> IO ()
printMadeGoal tasks pilots contents = do
    xs <- parseTracks contents
    case xs of
         Left msg -> print msg
         Right xs' ->
             putStr
             $ showPilotTracks 
             $ filterPilots pilots
             $ filterTasks tasks xs'
