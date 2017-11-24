{-# LANGUAGE DeriveDataTypeable #-}

module Cmd.Options (CmdOptions(..)) where

import System.Console.CmdArgs.Implicit (Data, Typeable)

-- | Options passed in on the command line.
data CmdOptions
    = CmdOptions { dir :: FilePath
                 -- ^ Picking all competition in this directory.
                 , file :: FilePath
                 -- ^ Picking the competition in this file.
                 , task :: [Int]
                 -- ^ Include only these tasks.
                 , pilot :: [String]
                 -- ^ Look only at these pilots
                 }
                 deriving (Data, Typeable, Show)
