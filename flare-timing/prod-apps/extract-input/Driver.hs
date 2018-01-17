{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Driver (driverMain) where

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)

import Flight.Cmd.Paths (checkPaths)
import Flight.Fsdb
    ( parseComp
    , parseNominal
    , parseTasks
    , parseTaskFolders
    , parseTracks
    )
import Flight.Comp
    ( FsdbFile(..)
    , FsdbXml(..)
    , CompSettings(..)
    , Comp(..)
    , Nominal(..)
    , Task(..)
    , TaskFolder(..)
    , PilotTrackLogFile(..)
    , fsdbToComp
    , findFsdb
    )
import Flight.Scribe (writeComp)
import Options (CmdOptions(..), mkOptions)

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
    files <- findFsdb o
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ go files
    end <- getTime Monotonic
    fprint ("Extracting tasks completed in " % timeSpecs % "\n") start end

go :: FsdbFile -> IO ()
go fsdbFile@(FsdbFile fsdbPath) = do
    contents <- readFile fsdbPath
    let contents' = dropWhile (/= '<') contents
    settings <- runExceptT $ fsdbSettings (FsdbXml contents')
    either print (writeComp (fsdbToComp fsdbFile)) settings

fsdbComp :: FsdbXml -> ExceptT String IO Comp
fsdbComp (FsdbXml contents) = do
    cs <- lift $ parseComp contents
    case cs of
        Left msg -> ExceptT . return $ Left msg
        Right [c] -> ExceptT . return $ Right c
        Right _ -> do
            let msg = "Expected only one comp"
            lift $ print msg
            throwE msg

fsdbNominal :: FsdbXml -> ExceptT String IO Nominal
fsdbNominal (FsdbXml contents) = do
    ns <- lift $ parseNominal contents
    case ns of
        Left msg -> ExceptT . return $ Left msg
        Right [n] -> ExceptT . return $ Right n
        _ -> do
            let msg = "Expected only one set of nominals for the comp"
            lift $ print msg
            throwE msg

fsdbTasks :: FsdbXml -> ExceptT String IO [Task]
fsdbTasks (FsdbXml contents) = do
    ts <- lift $ parseTasks contents
    ExceptT $ return ts

fsdbTaskFolders :: FsdbXml -> ExceptT String IO [TaskFolder]
fsdbTaskFolders (FsdbXml contents) = do
    fs <- lift $ parseTaskFolders contents
    ExceptT $ return fs

fsdbTracks :: FsdbXml -> ExceptT String IO [[PilotTrackLogFile]]
fsdbTracks (FsdbXml contents) = do
    fs <- lift $ parseTracks contents
    ExceptT $ return fs

fsdbSettings :: FsdbXml -> ExceptT String IO CompSettings
fsdbSettings fsdbXml = do
    c <- fsdbComp fsdbXml
    n <- fsdbNominal fsdbXml
    ts <- fsdbTasks fsdbXml
    fs <- fsdbTaskFolders fsdbXml
    tps <- fsdbTracks fsdbXml

    let msg =
            "Extracted "
            ++ show (length ts)
            ++ " tasks from \""
            ++ compName c
            ++ "\""

    lift . putStrLn $ msg
    return CompSettings { comp = c
                        , nominal = n
                        , tasks = ts
                        , taskFolders = fs
                        , pilots = tps
                        }
