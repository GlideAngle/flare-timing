{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cmd.Driver (driverMain) where

import Control.Monad (mapM_)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (takeFileName)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)

import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..), Detail(..))
import Data.Flight.Types (showTask)
import qualified Data.Flight.Nominal as N (parse)
import qualified Data.Flight.Waypoint as W (parse)
import qualified Data.Flight.Pilot as P
    ( Pilot(..), PilotTrackLogFile(..), parseNames, parseTracks)

driverMain :: IO ()
driverMain = withCmdArgs drive

showTaskPilots :: [ (Int, [ P.Pilot ]) ] -> [ String ]
showTaskPilots [] = [ "No tasks." ]
showTaskPilots xs =
    (\(i, pilots) -> "Task #" ++ show i ++ " pilots: " ++ show pilots) <$> xs

showPilots :: [[ P.Pilot ]] -> String
showPilots [] = "No pilots."
showPilots (comp : tasks) =
    unlines $ ("Comp pilots: " ++ show comp) : showTaskPilots (zip [ 1 .. ] tasks) 

showTaskPilotTracks :: [ (Int, [ P.PilotTrackLogFile ]) ] -> [ String ]
showTaskPilotTracks [] = [ "No tasks." ]
showTaskPilotTracks xs =
    (\(i, pilotTracks) -> "Task #" ++ show i ++ " pilot tracks: " ++ show pilotTracks) <$> xs

showPilotTracks :: [[ P.PilotTrackLogFile ]] -> String
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

            if null detail || Nominals `elem` detail
                then do
                    nominal <- N.parse contents'
                    case nominal of
                         Left msg -> print msg
                         Right nominal' -> print nominal'
                else
                   return ()

            if null detail || Pilots `elem` detail
               then do
                    pilots <- P.parseNames contents'
                    case pilots of
                         Left msg -> print msg
                         Right pilots' -> putStr $ showPilots pilots'
               else
                   return ()

            if null detail || Tasks `elem` detail
               then do
                    tasks <- W.parse contents'
                    case tasks of
                         Left msg -> print msg
                         Right tasks' -> print $ showTask <$> tasks'
               else
                   return ()

            if null detail || PilotTracks `elem` detail
               then do
                    pilotTracks <- P.parseTracks contents'
                    case pilotTracks of
                         Left msg -> print msg
                         Right pilotTracks' -> putStr $ showPilotTracks pilotTracks'
               else
                   return ()

