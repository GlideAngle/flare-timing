module Flight.TaskLength (readRoute, writeRoute, readRoutes) where

import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y

import Flight.Route (TaskTrack(..), cmpFields)
import Flight.Comp (CompInputFile, TaskLengthFile(..), taskToTaskLength)
import Flight.CompInput (compFileToTaskFiles)

readRoute :: (MonadThrow m, MonadIO m) => TaskLengthFile -> m (Maybe TaskTrack)
readRoute (TaskLengthFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeRoute :: TaskLengthFile -> Maybe TaskTrack -> IO ()
writeRoute (TaskLengthFile lenPath) route = do
    let cfg = Y.setConfCompare cmpFields Y.defConfig
    let yaml = Y.encodePretty cfg route
    BS.writeFile lenPath yaml

readRoutes :: CompInputFile -> IO [Maybe TaskTrack]
readRoutes compFile = do
    taskFiles <- compFileToTaskFiles compFile
    sequence
        [ do
            putStrLn $ "Reading task length from " ++ show routeFile
            readRoute routeFile
        | routeFile <- taskToTaskLength <$> taskFiles
        ]
