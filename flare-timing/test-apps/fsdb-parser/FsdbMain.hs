{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Control.Monad (mapM_, when)
import System.FilePath (takeFileName)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Fsdb
    ( parseComp
    , parseNominal
    , parseScoreBack
    , parseTasks
    , parsePilots
    , parseTracks
    , parseTaskFolders
    )
import qualified Flight.Comp as Comp
import Flight.Comp
    ( Pilot(..)
    , PilotTrackLogFile(..)
    , FsdbFile(..)
    , showTask
    , findFsdb
    )
import FsdbOptions (FsdbOptions(..), Detail(..), mkOptions)

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions name

    let lf = LenientFile {coerceFile = id}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

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

drive :: FsdbOptions -> IO ()
drive o = do
    files <- findFsdb o
    if null files then putStrLn "Couldn't find input files."
                  else mapM_ (go o) files

go :: FsdbOptions -> FsdbFile -> IO ()
go FsdbOptions{..} (FsdbFile path) = do
    putStrLn $ takeFileName path
    contents <- readFile path
    let contents' = dropWhile (/= '<') contents

    when (null detail || Comp `elem` detail) $ printComp contents'
    when (null detail || Nominals `elem` detail) $ printNominal contents'
    when (null detail || ScoreBack `elem` detail) $ printScoreBack contents'
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

printScoreBack :: String -> IO ()
printScoreBack contents = do
    sb <- parseScoreBack contents
    case sb of
         Left msg -> print msg
         Right sb' -> print sb'

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
    Right (sb : _) <- parseScoreBack contents
    Right (comp : _) <- parseComp contents
    tasks <- parseTasks (Comp.discipline comp) sb contents
    case tasks of
        Left msg -> print msg
        Right tasks' -> print $ showTask <$> tasks'

printComp :: String -> IO ()
printComp contents = do
    comp <- parseComp contents
    case comp of
         Left msg -> print msg
         Right comp' -> print comp'
