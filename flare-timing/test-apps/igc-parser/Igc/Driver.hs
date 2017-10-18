{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Igc.Driver (driverMain) where

import Control.Monad (mapM_)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (takeFileName)
import System.FilePath.Find (FileType(..), find, always, fileType, (==?))
import Igc.Args (withCmdArgs)
import Igc.Options (IgcOptions(..))
import Flight.Igc (parseFromFile)

drive :: IgcOptions -> IO ()
drive IgcOptions{..} = do
    dfe <- doesFileExist file
    if dfe then
        go file
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- find always (fileType ==? RegularFile) dir
            mapM_ go files
        else
            putStrLn "Couldn't find any IGC input files."
    where
        go path = do
            putStrLn $ takeFileName path
            p <- parseFromFile path
            case p of
                 Left msg -> print msg
                 Right p' -> print p'

driverMain :: IO ()
driverMain = withCmdArgs drive
