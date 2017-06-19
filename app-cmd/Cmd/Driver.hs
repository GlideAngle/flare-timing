{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cmd.Driver (driverMain) where

import Control.Monad (mapM_)
import Args (withCmdArgs)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (takeFileName)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import Options (DriveOptions(..))
import Data.Flight.Waypoint (parse)

drive :: DriveOptions -> IO ()
drive DriveOptions{..} = do
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

            p <- parse $ dropWhile (/= '<') contents
            case p of
                 Left msg -> print msg
                 Right p' -> print p'

driverMain :: IO ()
driverMain = withCmdArgs drive
