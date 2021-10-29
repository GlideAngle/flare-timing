module Flight.PegFrame
    ( readTaskPegFrame, writeTaskPegFrame
    , readCompPegFrame, writeCompPegFrame
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y
import Control.Concurrent.ParallelIO (parallel, parallel_)

import Flight.Track.Stop
    (TaskFraming(..), CompFraming(..), mkCompPegFrame, unMkCompPegFrame)
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    ( CompInputFile(..), PegFrameFile(..), compFileToTaskFiles, taskToPegFrame)

readTaskPegFrame :: (MonadThrow m, MonadIO m) => PegFrameFile -> m TaskFraming
readTaskPegFrame (PegFrameFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskPegFrame :: PegFrameFile -> TaskFraming -> IO ()
writeTaskPegFrame (PegFrameFile path) stopTask = do
    let cfg = Y.setConfCompare (fieldOrder stopTask) Y.defConfig
    let yaml = Y.encodePretty cfg stopTask
    BS.writeFile path yaml

readCompPegFrame :: CompInputFile -> IO CompFraming
readCompPegFrame compFile = do
    putStrLn "Reading pegged frames from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompPegFrame
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show pegFrameFile
                readTaskPegFrame pegFrameFile
            | pegFrameFile <- taskToPegFrame <$> taskFiles
            ]

writeCompPegFrame :: CompInputFile -> CompFraming -> IO ()
writeCompPegFrame compFile compPegFrames = do
    putStrLn "Writing pegged frames to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show pegFrameFile
            writeTaskPegFrame pegFrameFile taskPegFrames

        | taskPegFrames <- unMkCompPegFrame compPegFrames
        | pegFrameFile <- taskToPegFrame <$> taskFiles
        ]
