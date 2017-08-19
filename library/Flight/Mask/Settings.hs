module Flight.Mask.Settings (readSettings) where

import Control.Monad.Except (ExceptT(..), lift)
import System.FilePath (FilePath)
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)
import qualified Data.Flight.Comp as Cmp (CompSettings(..))

readSettings :: FilePath -> ExceptT String IO Cmp.CompSettings
readSettings compYamlPath = do
    contents <- lift $ BS.readFile compYamlPath
    ExceptT . return $ decodeEither contents
