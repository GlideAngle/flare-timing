{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cmd.Driver (driverMain) where

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Monad.Except (runExceptT)
import System.FilePath (takeFileName)

import Flight.Cmd.Paths (checkPaths)
import Cmd.Options (CmdOptions(..), mkOptions)
import Cmd.Settings (readCompSettings)
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS

import Flight.Field (FieldOrdering(..))
import Flight.Units ()
import Flight.Comp
    ( CompSettings(tasks)
    , Task(zones)
    , CompInputFile(..)
    , TaskLengthFile(..)
    , compToTaskLength
    , findCompInput
    )
import Flight.TaskTrack.Rational (taskTracks)
import Flight.Route (TaskTrack(..), TaskRoutes(..))

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
    settings <- runExceptT $ readCompSettings compPath
    either print f settings
    where
        f compInput = do
            let zs = zones <$> tasks compInput
            let includeTask = if null task then const True else flip elem task

            writeTaskLength
                (compToTaskLength compFile)
                (taskTracks noTaskWaypoints includeTask measure zs)

writeTaskLength :: TaskLengthFile -> [Maybe TaskTrack] -> IO ()
writeTaskLength (TaskLengthFile lenPath) os = 
    BS.writeFile lenPath yaml
    where
        taskLength = TaskRoutes { taskRoutes = os }
        cfg = Y.setConfCompare (fieldOrder taskLength) Y.defConfig
        yaml = Y.encodePretty cfg taskLength
