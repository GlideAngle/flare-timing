{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kml.Driver (driverMain) where

import Control.Monad (mapM_)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (takeFileName)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import Kml.Args (withCmdArgs)
import Kml.Options (KmlOptions(..))
import Flight.Kml (parse)

drive :: KmlOptions -> IO ()
drive KmlOptions{..} = do
    dfe <- doesFileExist file
    if dfe then
        go file
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- find always (fileType ==? RegularFile &&? extension ==? ".kml") dir
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
driverMain = withCmdArgs drive
