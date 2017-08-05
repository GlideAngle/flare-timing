{-# LANGUAGE DeriveDataTypeable #-}

module Cmd.Options (CmdOptions(..)) where

-- | Options passed in on the command line.
data CmdOptions
    = CmdOptions { dir :: FilePath
                 -- ^ Picking all competition in this directory.
                 , file :: FilePath
                 -- ^ Picking the competition in this file.
                 }
    deriving Show
