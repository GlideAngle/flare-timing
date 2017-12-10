{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cmd.Driver (driverMain) where

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find
    (FileType(..), (==?), (&&?), find, always, fileType, extension)

import Flight.Cmd.Paths (checkPaths)
import Cmd.Options (CmdOptions(..), mkOptions)
import Flight.Fsdb
    (parseComp, parseNominal, parseTasks, parseTaskFolders, parseTracks)
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS
import Flight.Comp
    ( FsdbFile(..)
    , FsdbXml(..)
    , CompFile(..)
    , CompSettings(..)
    , Comp(..)
    , Nominal(..)
    , Task(..)
    , TaskFolder(..)
    , PilotTrackLogFile(..)
    , fsdbToComp
    )

driverMain :: IO ()
driverMain = do
    name <- getProgName
    options <- cmdArgs $ mkOptions name
    err <- checkPaths options
    case err of
        Just msg -> putStrLn msg
        Nothing -> drive options

drive :: CmdOptions -> IO ()
drive CmdOptions{..} = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    dfe <- doesFileExist file
    if dfe then
        go $ FsdbFile file
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- find always (fileType ==? RegularFile &&? extension ==? ".fsdb") dir
            mapM_ go (FsdbFile <$> files)
        else
            putStrLn "Couldn't find any flight score competition database input files."
    end <- getTime Monotonic
    fprint ("Extracting tasks completed in " % timeSpecs % "\n") start end
    where
        go fsdbFile@(FsdbFile fsdbPath) = do
            contents <- readFile fsdbPath
            let contents' = dropWhile (/= '<') contents

            settings <- runExceptT $ fsdbSettings (FsdbXml contents')
            case settings of
                Left msg -> print msg
                Right cfg -> do
                    let yaml =
                            Y.encodePretty
                                (Y.setConfCompare cmp Y.defConfig)
                                cfg

                    let (CompFile compPath) = fsdbToComp fsdbFile

                    BS.writeFile compPath yaml

        cmp a b =
            case (a, b) of
                -- CompSettings fields
                ("comp", _) -> LT
                ("nominal", "comp") -> GT
                ("nominal", _) -> LT
                ("tasks", "taskFolders") -> LT
                ("tasks", "pilots") -> LT
                ("tasks", _) -> GT
                ("taskFolders", "pilots") -> LT
                ("taskFolders", _) -> GT
                ("pilots", _) -> GT
                -- Comp fields
                ("compName", _) -> LT
                ("location", "compName") -> GT
                ("location", _) -> LT
                ("from", "to") -> LT
                ("civilId", "utcOffset") -> LT
                ("civilId", _) -> GT
                ("utcOffset", _) -> GT
                -- Task fields
                ("taskName", _) -> LT
                ("zones", "taskName") -> GT
                ("zones", _) -> LT
                ("speedSection", "zoneTimes") -> LT
                ("speedSection", "startGates") -> LT
                ("speedSection", _) -> GT
                ("zoneTimes", "startGates") -> LT
                ("zoneTimes", _) -> GT
                ("startGates", _) -> GT
                ("open", _) -> LT
                ("close", _) -> GT
                -- Turnpoint fields
                ("zoneName", _) -> LT
                ("lat", "zoneName") -> GT
                ("lat", _) -> LT
                ("lng", "zoneName") -> GT
                ("lng", "lat") -> GT
                ("lng", _) -> LT
                ("radius", _) -> GT
                _ -> compare a b

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
    ps <- fsdbTracks fsdbXml
    let msg = "Extracted " ++ show (length ts) ++ " tasks from \"" ++ compName c ++ "\""
    lift . putStrLn $ msg
    return CompSettings { comp = c
                        , nominal = n
                        , tasks = ts
                        , taskFolders = fs
                        , pilots = ps
                        }
