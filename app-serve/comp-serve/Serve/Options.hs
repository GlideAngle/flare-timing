module Serve.Options (ServeOptions(..)) where

-- | Options passed in on the command line.
newtype ServeOptions
  = ServeOptions { file :: FilePath
                 -- ^ Serve the competition in this file.
                 }
    deriving Show
