{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TaskLengthDriver (driverMain) where

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Monad.Except (runExceptT)
import System.FilePath (takeFileName)

import Flight.Cmd.Paths (checkPaths)
import Flight.Units ()
import Flight.Comp
    ( CompSettings(tasks)
    , Task(zones)
    , CompInputFile(..)
    , compToTaskLength
    , findCompInput
    )
import Flight.Route (TaskRoute(..))
import Flight.TaskTrack.Rational (taskTracks)
import Flight.Scribe (readComp, writeRoute)
import TaskLengthOptions (CmdOptions(..), mkOptions)

driverMain :: IO ()
driverMain = do
    name <- getProgName
    options <- cmdArgs $ mkOptions name
    err <- checkPaths options
    maybe (drive options) putStrLn err

drive :: CmdOptions -> IO ()
drive o = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findCompInput o
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    fprint ("Measuring task lengths completed in " % timeSpecs % "\n") start end

go :: CmdOptions -> CompInputFile -> IO ()
go CmdOptions{..} compFile@(CompInputFile compPath) = do
    putStrLn $ takeFileName compPath
    settings <- runExceptT $ readComp compFile
    either print f settings
    where
        f compInput = do
            let zs = zones <$> tasks compInput
            let includeTask = if null task then const True else flip elem task

            writeRoute
                (compToTaskLength compFile)
                (TaskRoute $ taskTracks noTaskWaypoints includeTask measure zs)
