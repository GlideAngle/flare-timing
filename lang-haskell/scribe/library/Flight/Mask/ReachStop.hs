module Flight.Mask.ReachStop
    ( readTaskMaskReachStop, writeTaskMaskReachStop
    , readCompMaskReachStop, writeCompMaskReachStop
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y
import Control.Concurrent.ParallelIO (parallel, parallel_)

import Flight.Track.Mask
    (TaskMaskingReach(..), CompMaskingReach(..), mkCompMaskReach, unMkCompMaskReach)
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    (CompInputFile(..), MaskReachStopFile(..), compFileToTaskFiles, taskToMaskReachStop)

readTaskMaskReachStop :: MonadIO m => MaskReachStopFile -> m TaskMaskingReach
readTaskMaskReachStop (MaskReachStopFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskMaskReachStop :: MaskReachStopFile -> TaskMaskingReach -> IO ()
writeTaskMaskReachStop (MaskReachStopFile path) bonusReach = do
    let cfg = Y.setConfCompare (fieldOrder bonusReach) Y.defConfig
    let yaml = Y.encodePretty cfg bonusReach
    BS.writeFile path yaml

readCompMaskReachStop :: CompInputFile -> IO CompMaskingReach
readCompMaskReachStop compFile = do
    putStrLn "Reading bonus reach (for stopped tasks) from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompMaskReach
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show maskReachStopFile
                readTaskMaskReachStop maskReachStopFile
            | maskReachStopFile <- taskToMaskReachStop <$> taskFiles
            ]

writeCompMaskReachStop :: CompInputFile -> CompMaskingReach -> IO ()
writeCompMaskReachStop compFile compMaskReachStop = do
    putStrLn "Writing bonus reach (for stopped tasks) to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show maskReachStopFile
            writeTaskMaskReachStop maskReachStopFile taskMaskReachStop

        | taskMaskReachStop <- unMkCompMaskReach compMaskReachStop
        | maskReachStopFile <- taskToMaskReachStop <$> taskFiles
        ]
