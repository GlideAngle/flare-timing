{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Flight.Comp
    (Comp(..), Nominal(..), Task(..), TaskFolder(..), PilotTrackLogFile(..))
import qualified Data.Yaml.Pretty as Y
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.ByteString as BS

-- SEE: https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md
liftEither :: Monad m => Either e a -> ExceptT e m a
liftEither x = ExceptT (return x)

data CompSettings =
    CompSettings { comp :: Comp
                 , nominal :: Nominal
                 , tasks :: [Task]
                 , taskFolders :: [TaskFolder]
                 , pilots :: [[PilotTrackLogFile]]
                 } deriving (Show, Generic)

instance ToJSON CompSettings
instance FromJSON CompSettings

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
            let yamlPath = replaceExtension fsdbPath ".yaml"

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
        Right [c] -> liftEither $ Right c
        _ -> do
            let msg = "Expected only one comp"
            lift $ print msg
            throwE msg

fsdbNominal :: String -> ExceptT String IO Nominal
fsdbNominal contents = do
    ns <- lift $ parseNominal contents
    case ns of
        Right [n] -> liftEither $ Right n
        _ -> do
            let msg = "Expected only one set of nominals for the comp"
            lift $ print msg
            throwE msg

fsdbTasks :: String -> ExceptT String IO [Task]
fsdbTasks contents = do
    ws <- lift $ parseTasks contents
    liftEither ws

fsdbTaskFolders :: String -> ExceptT String IO [TaskFolder]
fsdbTaskFolders contents = do
    ws <- lift $ parseTaskFolders contents
    liftEither ws

fsdbTracks :: String -> ExceptT String IO [[PilotTrackLogFile]]
fsdbTracks contents = do
    ws <- lift $ parseTracks contents
    liftEither ws

fsdbSettings :: String -> ExceptT String IO CompSettings
fsdbSettings contents = do
    c <- fsdbComp contents
    n <- fsdbNominal contents
    ws <- fsdbTasks contents
    fs <- fsdbTaskFolders contents
    ps <- fsdbTracks contents
    return CompSettings { comp = c
                        , nominal = n
                        , tasks = ws
                        , taskFolders = fs
                        , pilots = ps
                        }
