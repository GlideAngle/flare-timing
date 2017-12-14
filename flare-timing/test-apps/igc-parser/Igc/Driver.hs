{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Igc.Driver (driverMain) where

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Control.Monad (mapM_)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (takeFileName)

import Flight.Cmd.Paths (checkPaths)
import Igc.Options (IgcOptions(..), mkOptions)
import Flight.Igc (parseFromFile)
import Flight.Comp (FileType(Igc), findFiles)

drive :: IgcOptions -> IO ()
drive IgcOptions{..} = do
    dfe <- doesFileExist file
    if dfe then
        go file
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- findFiles Igc dir
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
driverMain = do
    name <- getProgName
    options <- cmdArgs $ mkOptions name
    err <- checkPaths options
    maybe (drive options) putStrLn err
