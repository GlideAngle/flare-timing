{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cmd.Driver (driverMain) where

import Data.Maybe (catMaybes)
import Control.Monad (mapM_)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (FilePath, (</>), takeFileName, takeDirectory)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)

import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..), Reckon(..))
import Data.Flight.Pilot
    ( Pilot(..), PilotTrackLogFile(..), TrackLogFile(..), parseTracks)

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
                Goal ->
                    printMadeGoal
                        (takeDirectory path)
                        task
                        (Pilot <$> pilot)
                        contents'

                x ->
                    putStrLn $ "TODO: Handle other reckon of " ++ show x

goalPilotTrack :: PilotTrackLogFile -> IO (Pilot, Bool)
goalPilotTrack (PilotTrackLogFile p Nothing) = return (p, False)
goalPilotTrack (PilotTrackLogFile p (Just (TrackLogFile file))) = do
    dfe <- doesFileExist file
    return (p, dfe)

goalTaskPilotTracks :: [ (Int, [ PilotTrackLogFile ]) ] -> IO [ String ]
goalTaskPilotTracks [] = return [ "No tasks." ]
goalTaskPilotTracks xs = do
    zs <- sequence $ (\(i, pilotTracks) -> do
                ys <- sequence $ goalPilotTrack <$> pilotTracks
                return $ "Task #"
                         ++ show i
                         ++ " pilot tracks: "
                         ++ (unlines $ show <$> ys))
                <$> xs
    return $ zs

goalPilotTracks :: [[ PilotTrackLogFile ]] -> IO String
goalPilotTracks [] = return "No pilots."
goalPilotTracks tasks = do
    xs <- goalTaskPilotTracks (zip [ 1 .. ] tasks) 
    return $ unlines xs

filterPilots :: [ Pilot ]
             -> [[ PilotTrackLogFile ]]
             -> [[ PilotTrackLogFile ]]

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
            -> [[ PilotTrackLogFile ]]

filterTasks [] xs = xs
filterTasks tasks xs =
    zipWith (\i ys ->
        if i `elem` tasks then ys else []) [ 1 .. ] xs

makeAbsolute :: FilePath -> Task -> PilotTrackLogFile -> PilotTrackLogFile
makeAbsolute _ _ x@(PilotTrackLogFile _ Nothing) = x
makeAbsolute dir task (PilotTrackLogFile p (Just (TrackLogFile file))) =
    PilotTrackLogFile p (Just (TrackLogFile file'))
    where
        -- TODO: Get the track log folder for each task.
        file' = dir </> "Tracklogs" </> "day " ++ show task </> file

printMadeGoal :: FilePath -> [ Task ] -> [ Pilot ] -> String -> IO ()
printMadeGoal dir tasks pilots contents = do
    xs <- parseTracks contents
    case xs of
         Left msg -> print msg
         Right xs' -> do
             let ys = filterPilots pilots $ filterTasks tasks xs'
             let fs = (makeAbsolute dir) <$> [ 1 .. length ys ]
             let zs = zipWith (\f y -> f <$> y) fs ys
             s <- goalPilotTracks zs
             putStr s
