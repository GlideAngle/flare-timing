module Kml.Options (KmlOptions(..)) where

data KmlOptions
    = KmlOptions { dir :: FilePath
                   , file :: FilePath
                   }
    deriving Show
