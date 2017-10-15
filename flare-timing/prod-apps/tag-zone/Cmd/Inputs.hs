module Cmd.Inputs (readCrossings) where

import Control.Monad.Except (ExceptT(..), lift)
import System.FilePath (FilePath)
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)
import Data.Flight.PilotTrack (PilotCrossings)

readCrossings :: FilePath -> ExceptT String IO PilotCrossings
readCrossings yamlPath = do
    contents <- lift $ BS.readFile yamlPath
    ExceptT . return $ decodeEither contents
