{-# LANGUAGE DeriveDataTypeable #-}

module Cmd.Options (CmdOptions(..), Detail(..)) where

import System.Console.CmdArgs.Implicit (Default(..), Data)

data CmdOptions
    = CmdOptions { dir :: FilePath
                 , file :: FilePath
                 , detail :: [Detail]
                 }
    deriving Show

data Detail
    = Tasks
    | Nominals
    deriving (Data, Eq, Show)
