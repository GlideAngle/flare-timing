{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kml.Driver (driverMain) where

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Control.Monad (mapM_)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (takeFileName)
import Kml.Args (checkOptions)
import Kml.Options (KmlOptions(..), mkOptions)
import Flight.Kml (parse)

import Flight.Comp (FileType(Kml), findFiles)

drive :: KmlOptions -> IO ()
drive KmlOptions{..} = do
    dfe <- doesFileExist file
    if dfe then
        go file
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- findFiles Kml dir
            mapM_ go files
        else
            putStrLn "Couldn't find any KML input files."
    where
        go path = do
            putStrLn $ takeFileName path
            contents <- readFile path
            let contents' = dropWhile (/= '<') contents
            p <- parse contents' 
            case p of
                 Left msg -> print msg
                 Right p' -> print p'

driverMain :: IO ()
driverMain = do
    name <- getProgName
    options <- cmdArgs $ mkOptions name
    err <- checkOptions options
    case err of
        Just msg -> putStrLn msg
        Nothing -> drive options
