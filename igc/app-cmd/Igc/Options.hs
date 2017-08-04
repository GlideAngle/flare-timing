module Igc.Options (IgcOptions(..)) where

-- | Options passed in on the command line.
data IgcOptions
    = IgcOptions { dir :: FilePath
                 -- ^ Picking all competition in this directory.
                 , file :: FilePath
                 -- ^ Picking the competition in this file.
                 }
    deriving Show
