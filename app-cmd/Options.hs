module Options (DriveOptions(..)) where

data DriveOptions
    = DriveOptions { dir :: FilePath
                   , file :: FilePath
                   }
    deriving Show
