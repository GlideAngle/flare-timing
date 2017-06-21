module Igc.Options (IgcOptions(..)) where

data IgcOptions
    = IgcOptions { dir :: FilePath
                 , file :: FilePath
                 }
    deriving Show
