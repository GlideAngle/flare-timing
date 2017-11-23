module Cmd.Inputs (readTags) where

import Control.Monad.Except (ExceptT(..), lift)
import System.FilePath (FilePath)
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)
import Flight.Track.Tag (Tagging)

readTags :: FilePath -> ExceptT String IO Tagging
readTags yamlPath = do
    contents <- lift $ BS.readFile yamlPath
    ExceptT . return $ decodeEither contents
