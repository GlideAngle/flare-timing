module Flight.TagZone
    ( readTaskTagZone, writeTaskTagZone
    , readCompTagZone, writeCompTagZone
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y
import Control.Concurrent.ParallelIO (parallel, parallel_)

import Flight.Track.Tag
    (TaskTagging(..), CompTagging(..), mkCompTagZone, unMkCompTagZone)
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    (CompInputFile(..), TagZoneFile(..), compFileToTaskFiles, taskToTagZone)

readTaskTagZone :: (MonadThrow m, MonadIO m) => TagZoneFile -> m TaskTagging
readTaskTagZone (TagZoneFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskTagZone :: TagZoneFile -> TaskTagging -> IO ()
writeTaskTagZone (TagZoneFile path) tagZone = do
    let cfg = Y.setConfCompare (fieldOrder tagZone) Y.defConfig
    let yaml = Y.encodePretty cfg tagZone
    BS.writeFile path yaml

readCompTagZone :: CompInputFile -> IO CompTagging
readCompTagZone compFile = do
    putStrLn "Reading zone taggings from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompTagZone
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show tagZoneFile
                readTaskTagZone tagZoneFile
            | tagZoneFile <- taskToTagZone <$> taskFiles
            ]

writeCompTagZone :: CompInputFile -> CompTagging -> IO ()
writeCompTagZone compFile compTagZones = do
    putStrLn "Writing zone taggings to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show tagZoneFile
            writeTaskTagZone tagZoneFile taskTagZones

        | taskTagZones <- unMkCompTagZone compTagZones
        | tagZoneFile <- taskToTagZone <$> taskFiles
        ]
