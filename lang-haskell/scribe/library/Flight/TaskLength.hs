module Flight.TaskLength
    ( readRoute, writeRoute
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y

import Flight.Route (TaskTrack(..), cmpFields)
import Flight.Comp (TaskLengthFile(..))

readRoute :: (MonadThrow m, MonadIO m) => TaskLengthFile -> m (Maybe TaskTrack)
readRoute (TaskLengthFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeRoute :: TaskLengthFile -> Maybe TaskTrack -> IO ()
writeRoute (TaskLengthFile lenPath) route =
    BS.writeFile lenPath yaml
    where
        cfg = Y.setConfCompare cmpFields Y.defConfig
        yaml = Y.encodePretty cfg route
