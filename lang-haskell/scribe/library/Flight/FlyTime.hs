module Flight.FlyTime
    ( readTaskFlying, writeTaskFlying
    , readCompFlying, writeCompFlying
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y
import Control.Concurrent.ParallelIO (parallel, parallel_)

import Flight.Track.Cross
    (TaskFlying, CompFlying, mkCompFlyingTime, unMkCompFlyingTime)
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    ( CompInputFile(..), FlyTimeFile(..)
    , taskToFlyTime, compFileToTaskFiles
    )

readTaskFlying :: (MonadThrow m, MonadIO m) => FlyTimeFile -> m TaskFlying
readTaskFlying (FlyTimeFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskFlying :: FlyTimeFile -> TaskFlying -> IO ()
writeTaskFlying (FlyTimeFile path) flyTime = do
    let cfg = Y.setConfCompare (fieldOrder flyTime) Y.defConfig
    let yaml = Y.encodePretty cfg flyTime
    BS.writeFile path yaml

readCompFlying :: CompInputFile -> IO CompFlying
readCompFlying compFile = do
    putStrLn "Reading flying times from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompFlyingTime
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show flyingFile
                readTaskFlying flyingFile
            | flyingFile <- taskToFlyTime <$> taskFiles
            ]

writeCompFlying :: CompInputFile -> CompFlying -> IO ()
writeCompFlying compFile compFlyingTimes = do
    putStrLn "Writing flying times to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show flyingFile
            writeTaskFlying flyingFile taskFlyingTimes

        | taskFlyingTimes <- unMkCompFlyingTime compFlyingTimes
        | flyingFile <- taskToFlyTime <$> taskFiles
        ]
