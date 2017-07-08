{-# LANGUAGE DeriveDataTypeable #-}

module Cmd.Options (CmdOptions(..), Detail(..)) where

import System.Console.CmdArgs.Implicit (Data)

data CmdOptions
    = CmdOptions { dir :: FilePath
                 , file :: FilePath
                 , detail :: [Detail]
                 }
    deriving Show

data Detail
    = Nominals
    | Pilots
    | Tasks
    | TaskFolders
    | PilotTracks
    deriving (Data, Eq, Show)
