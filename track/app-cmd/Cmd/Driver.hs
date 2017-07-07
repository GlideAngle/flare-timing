{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cmd.Driver (driverMain) where

import Control.Monad (mapM_)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (takeFileName)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)

import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..), Reckon(..))

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
        go path = do
            putStrLn $ takeFileName path
            contents <- readFile path
            let contents' = dropWhile (/= '<') contents

            case reckon of
                Goal -> printMadeGoal contents'
                x -> putStrLn $ "TODO: Handle other reckon of " ++ show x

printMadeGoal :: String -> IO ()
printMadeGoal _ =
    putStrLn "TODO: Made goal."
