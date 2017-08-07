{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cmd.Driver (driverMain) where

import Control.Monad (mapM_)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (takeFileName, replaceExtension)
import System.FilePath.Find
    (FileType(..), (==?), (&&?), find, always, fileType, extension)

import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..))
import Data.Flight.Fsdb
    (parseComp, parseNominal, parseTasks, parseTaskFolders, parseTracks)
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS
import Data.Flight.Comp
    ( CompSettings(..)
    , Comp(..)
    , Nominal(..)
    , Task(..)
    , TaskFolder(..)
    , PilotTrackLogFile(..)
    )

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
        go fsdbPath = do
            putStrLn $ takeFileName fsdbPath
            contents <- readFile fsdbPath
            let contents' = dropWhile (/= '<') contents
            let yamlPath = replaceExtension fsdbPath ".comp.yaml"

            settings <- runExceptT $ fsdbSettings contents'
            case settings of
                Left msg -> print msg
                Right cfg -> do
                    let yaml =
                            Y.encodePretty
                                (Y.setConfCompare cmp Y.defConfig)
                                cfg

                    BS.writeFile yamlPath yaml

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
                ("speedSection", _) -> GT
                -- Turnpoint fields
                ("zoneName", _) -> LT
                ("lat", "zoneName") -> GT
                ("lat", _) -> LT
                ("lng", "zoneName") -> GT
                ("lng", "lat") -> GT
                ("lng", _) -> LT
                ("radius", _) -> GT
                _ -> compare a b

fsdbComp :: String -> ExceptT String IO Comp
fsdbComp contents = do
    cs <- lift $ parseComp contents
    case cs of
        Right [c] -> ExceptT . return $ Right c
        _ -> do
            let msg = "Expected only one comp"
            lift $ print msg
            throwE msg

fsdbNominal :: String -> ExceptT String IO Nominal
fsdbNominal contents = do
    ns <- lift $ parseNominal contents
    case ns of
        Right [n] -> ExceptT . return $ Right n
        _ -> do
            let msg = "Expected only one set of nominals for the comp"
            lift $ print msg
            throwE msg

fsdbTasks :: String -> ExceptT String IO [Task]
fsdbTasks contents = do
    ts <- lift $ parseTasks contents
    lift $ print ts
    ExceptT $ return ts

fsdbTaskFolders :: String -> ExceptT String IO [TaskFolder]
fsdbTaskFolders contents = do
    fs <- lift $ parseTaskFolders contents
    ExceptT $ return fs

fsdbTracks :: String -> ExceptT String IO [[PilotTrackLogFile]]
fsdbTracks contents = do
    fs <- lift $ parseTracks contents
    ExceptT $ return fs

fsdbSettings :: String -> ExceptT String IO CompSettings
fsdbSettings contents = do
    c <- fsdbComp contents
    n <- fsdbNominal contents
    ts <- fsdbTasks contents
    fs <- fsdbTaskFolders contents
    ps <- fsdbTracks contents
    return CompSettings { comp = c
                        , nominal = n
                        , tasks = ts
                        , taskFolders = fs
                        , pilots = ps
                        }
