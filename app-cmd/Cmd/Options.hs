module Cmd.Options (CmdOptions(..)) where

data CmdOptions
    = CmdOptions { dir :: FilePath
                 , file :: FilePath
                 }
    deriving Show
