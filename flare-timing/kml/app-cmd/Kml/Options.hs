module Kml.Options (KmlOptions(..)) where

-- | Options passed in on the command line.
data KmlOptions
    = KmlOptions { dir :: FilePath
                 -- ^ Picking all competition in this directory.
                 , file :: FilePath
                 -- ^ Picking the competition in this file.
                 }
    deriving Show
