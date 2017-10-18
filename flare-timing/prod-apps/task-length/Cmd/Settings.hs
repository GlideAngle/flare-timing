module Cmd.Settings (readCompSettings) where

import Control.Monad.Except (ExceptT(..), lift)
import System.FilePath (FilePath)
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)
import Flight.Comp (CompSettings(..))

readCompSettings :: FilePath -> ExceptT String IO CompSettings
readCompSettings compYamlPath = do
    contents <- lift $ BS.readFile compYamlPath
    ExceptT . return $ decodeEither contents
