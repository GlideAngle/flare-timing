module Flight.CrossZone
    ( readTaskCrossing, writeTaskCrossing
    , readCompCrossing, writeCompCrossing
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

readTaskCrossing :: (MonadThrow m, MonadIO m) => CrossZoneFile -> m TaskCrossing
readTaskCrossing (CrossZoneFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskCrossing :: CrossZoneFile -> TaskCrossing -> IO ()
writeTaskCrossing (CrossZoneFile path) crossZone = do
    let cfg = Y.setConfCompare (fieldOrder crossZone) Y.defConfig
    let yaml = Y.encodePretty cfg crossZone
    BS.writeFile path yaml

readCompCrossing :: CompInputFile -> IO CompCrossing
readCompCrossing compFile = do
    putStrLn "Reading zone crossings from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompCrossZone
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show crossZoneFile
                readTaskCrossing crossZoneFile
            | crossZoneFile <- taskToCrossZone <$> taskFiles
            ]

writeCompCrossing :: CompInputFile -> CompCrossing -> IO ()
writeCompCrossing compFile compCrossingTimes = do
    putStrLn "Writing zone crossings to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show crossZoneFile
            writeTaskCrossing crossZoneFile taskCrossingTimes

        | taskCrossingTimes <- unMkCompCrossZone compCrossingTimes
        | crossZoneFile <- taskToCrossZone <$> taskFiles
        ]
