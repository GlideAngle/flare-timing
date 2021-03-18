module Flight.TaskLength
    ( readAltRoute, writeAltRoute
    , readRoute, writeRoute
    , readRoutes, writeRoutes
    ) where

import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y
import Control.Concurrent.ParallelIO (parallel, parallel_)

import Flight.Field (FieldOrdering(..))
import Flight.Route (GeoLines, TaskTrack(..), cmpFields)
import Flight.Comp
    ( AltRouteFile(..), CompInputFile, TaskLengthFile(..)
    , taskToTaskLength, compFileToTaskFiles
    )

readAltRoute :: (MonadThrow m, MonadIO m) => AltRouteFile -> m [GeoLines]
readAltRoute (AltRouteFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeAltRoute :: AltRouteFile -> [GeoLines] -> IO ()
writeAltRoute (AltRouteFile path) track = do
    let cfg = Y.setConfCompare (fieldOrder track) Y.defConfig
    let yaml = Y.encodePretty cfg track

    createDirectoryIfMissing True $ takeDirectory path
    BS.writeFile path yaml

readRoute :: (MonadThrow m, MonadIO m) => TaskLengthFile -> m (Maybe TaskTrack)
readRoute (TaskLengthFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeRoute :: TaskLengthFile -> Maybe TaskTrack -> IO ()
writeRoute (TaskLengthFile lenPath) route = do
    let cfg = Y.setConfCompare cmpFields Y.defConfig
    let yaml = Y.encodePretty cfg route
    BS.writeFile lenPath yaml

readRoutes :: CompInputFile -> IO [Maybe TaskTrack]
readRoutes compFile = do
    putStrLn "Reading task lengths from:"
    taskFiles <- compFileToTaskFiles compFile
    parallel
        [ do
            putStrLn $ "\t" ++ show routeFile
            readRoute routeFile
        | routeFile <- taskToTaskLength <$> taskFiles
        ]

writeRoutes :: CompInputFile -> [Maybe TaskTrack] -> IO ()
writeRoutes compFile routes = do
    putStrLn "Writing task lengths to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show routeFile
            writeRoute routeFile route

        | route <- routes
        | routeFile <- taskToTaskLength <$> taskFiles
        ]
