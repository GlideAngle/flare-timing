module Serve.Options (ServeOptions(..)) where

newtype ServeOptions = ServeOptions { file :: FilePath } deriving Show
