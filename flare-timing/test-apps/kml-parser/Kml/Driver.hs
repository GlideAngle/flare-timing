{-# LANGUAGE OverloadedStrings #-}

module Kml.Driver (driverMain) where

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Control.Monad (mapM_)
import System.FilePath (takeFileName)

import Flight.Cmd.Paths (checkPaths)
import Kml.Options (KmlOptions(..), mkOptions)
import Flight.Kml (parse)

import Flight.Comp (KmlFile(..), findKml)

driverMain :: IO ()
driverMain = do
    name <- getProgName
    options <- cmdArgs $ mkOptions name
    err <- checkPaths options
    maybe (drive options) putStrLn err

drive :: KmlOptions -> IO ()
drive o = do
    files <- findKml o
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ go files

go :: KmlFile -> IO ()
go (KmlFile path) = do
    putStrLn $ takeFileName path
    contents <- readFile path
    let contents' = dropWhile (/= '<') contents
    p <- parse contents'
    either print print p
