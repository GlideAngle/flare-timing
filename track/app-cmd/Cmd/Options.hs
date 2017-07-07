{-# LANGUAGE DeriveDataTypeable #-}

module Cmd.Options (CmdOptions(..), Reckon(..)) where

import System.Console.CmdArgs.Implicit (Default(..), Data, Typeable)

data CmdOptions
    = CmdOptions { dir :: FilePath
                 , file :: FilePath
                 , task :: [Int]
                 , pilot :: [String]
                 , reckon :: Reckon
                 }
                 deriving (Data, Typeable, Show)

data Reckon
    = Goal
    | Zone
    | Distance
    | Time 
    | Lead
    deriving (Data, Typeable, Eq, Show)

instance Default Reckon where
    def = Goal
