module Cmd.Inputs (readTags) where

import Control.Monad.Except (ExceptT(..), lift)
import System.FilePath (FilePath)
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)
import Data.Flight.PilotTrack (PilotTags)

readTags :: FilePath -> ExceptT String IO PilotTags
readTags yamlPath = do
    contents <- lift $ BS.readFile yamlPath
    ExceptT . return $ decodeEither contents
