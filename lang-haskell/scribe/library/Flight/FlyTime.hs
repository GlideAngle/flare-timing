module Flight.FlyTime
    ( readTaskFlyTime, writeTaskFlyTime
    , readCompFlyTime, writeCompFlyTime
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y
import Control.Concurrent.ParallelIO (parallel, parallel_)

import Flight.Track.Cross
    (TaskFlying, CompFlying, mkCompFlyTime, unMkCompFlyTime)
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    ( CompInputFile(..), FlyTimeFile(..)
    , taskToFlyTime, compFileToTaskFiles
    )

readTaskFlyTime :: (MonadThrow m, MonadIO m) => FlyTimeFile -> m TaskFlying
readTaskFlyTime (FlyTimeFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskFlyTime :: FlyTimeFile -> TaskFlying -> IO ()
writeTaskFlyTime (FlyTimeFile path) flyTime = do
    let cfg = Y.setConfCompare (fieldOrder flyTime) Y.defConfig
    let yaml = Y.encodePretty cfg flyTime
    BS.writeFile path yaml

readCompFlyTime :: CompInputFile -> IO CompFlying
readCompFlyTime compFile = do
    putStrLn "Reading flying times from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompFlyTime
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show flyTimeFile
                readTaskFlyTime flyTimeFile
            | flyTimeFile <- taskToFlyTime <$> taskFiles
            ]

writeCompFlyTime :: CompInputFile -> CompFlying -> IO ()
writeCompFlyTime compFile compFlyingTimes = do
    putStrLn "Writing flying times to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show flyTimeFile
            writeTaskFlyTime flyTimeFile taskFlyingTimes

        | taskFlyingTimes <- unMkCompFlyTime compFlyingTimes
        | flyTimeFile <- taskToFlyTime <$> taskFiles
        ]
