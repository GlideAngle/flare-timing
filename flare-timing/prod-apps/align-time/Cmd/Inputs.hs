module Cmd.Inputs (readTaggings) where

import Control.Monad.Except (ExceptT(..), lift)
import System.FilePath (FilePath)
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)
import Flight.Track.Tag (Tagging)

readTaggings :: FilePath -> ExceptT String IO Tagging
readTaggings yamlPath = do
    contents <- lift $ BS.readFile yamlPath
    ExceptT . return $ decodeEither contents
