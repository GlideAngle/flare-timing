{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Cmd.Driver (driverMain) where

import Data.String (IsString)
import Control.Monad (mapM_)
import Control.Monad.Except (runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (FilePath, takeFileName, replaceExtension, dropExtension)
import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..))
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS

import Flight.Units ()
import Flight.Mask.Task (taskTracks)
import qualified Data.Flight.TrackZone as TZ
    (TaskTrack(..), TaskTracks(..))

driverMain :: IO ()
driverMain = withCmdArgs drive

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        ("pointToPoint", _) -> LT
        ("edgeToEdge", _) -> GT
        ("lat", _) -> LT
        ("lng", _) -> GT
        ("distance", _) -> LT
        ("legs", "distance") -> GT
        ("legs", _) -> LT
        ("wayPoints", _) -> GT
        _ -> compare a b

drive :: CmdOptions -> IO ()
drive CmdOptions{..} = do
    dfe <- doesFileExist file
    if dfe then
        withFile file
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- find always (fileType ==? RegularFile &&? extension ==? ".comp-input.yaml") dir
            mapM_ withFile files
        else
            putStrLn "Couldn't find any flight score competition yaml input files."
    where
        withFile yamlCompPath = do
            putStrLn $ takeFileName yamlCompPath
            let yamlMaskPath =
                    flip replaceExtension ".task-length.yaml"
                    $ dropExtension yamlCompPath
            ts <- runExceptT $ taskTracks noTaskWaypoints measure yamlCompPath
            case ts of
                Left msg -> print msg
                Right ts' -> writeTaskLength ts' yamlMaskPath

            where
                writeTaskLength :: [TZ.TaskTrack] -> FilePath -> IO ()
                writeTaskLength os yamlPath = do
                    let tzi =
                            TZ.TaskTracks { taskTracks = os }

                    let yaml =
                            Y.encodePretty
                                (Y.setConfCompare cmp Y.defConfig)
                                tzi 

                    BS.writeFile yamlPath yaml
