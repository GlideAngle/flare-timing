module Flight.CrossZone
    ( readTaskCrossZone, writeTaskCrossZone
    , readCompCrossZone, writeCompCrossZone
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y
import Control.Concurrent.ParallelIO (parallel, parallel_)

import Flight.Track.Cross
    (TaskCrossing, CompCrossing, mkCompCrossZone, unMkCompCrossZone)
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    (CompInputFile(..), CrossZoneFile(..), compFileToTaskFiles, taskToCrossZone)

readTaskCrossZone :: (MonadThrow m, MonadIO m) => CrossZoneFile -> m TaskCrossing
readTaskCrossZone (CrossZoneFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskCrossZone :: CrossZoneFile -> TaskCrossing -> IO ()
writeTaskCrossZone (CrossZoneFile path) crossZone = do
    let cfg = Y.setConfCompare (fieldOrder crossZone) Y.defConfig
    let yaml = Y.encodePretty cfg crossZone
    BS.writeFile path yaml

readCompCrossZone :: CompInputFile -> IO CompCrossing
readCompCrossZone compFile = do
    putStrLn "Reading zone crossings from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompCrossZone
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show crossZoneFile
                readTaskCrossZone crossZoneFile
            | crossZoneFile <- taskToCrossZone <$> taskFiles
            ]

writeCompCrossZone :: CompInputFile -> CompCrossing -> IO ()
writeCompCrossZone compFile compCrossingTimes = do
    putStrLn "Writing zone crossings to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show crossZoneFile
            writeTaskCrossZone crossZoneFile taskCrossingTimes

        | taskCrossingTimes <- unMkCompCrossZone compCrossingTimes
        | crossZoneFile <- taskToCrossZone <$> taskFiles
        ]
