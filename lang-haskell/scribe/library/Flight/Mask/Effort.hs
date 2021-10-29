module Flight.Mask.Effort
    ( readTaskMaskEffort, writeTaskMaskEffort
    , readCompMaskEffort, writeCompMaskEffort
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y
import Control.Concurrent.ParallelIO (parallel, parallel_)

import Flight.Track.Mask
    (TaskMaskingEffort(..), CompMaskingEffort(..), mkCompMaskEffort, unMkCompMaskEffort)
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    (CompInputFile(..), MaskEffortFile(..), compFileToTaskFiles, taskToMaskEffort)

readTaskMaskEffort :: MonadIO m => MaskEffortFile -> m TaskMaskingEffort
readTaskMaskEffort (MaskEffortFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskMaskEffort :: MaskEffortFile -> TaskMaskingEffort -> IO ()
writeTaskMaskEffort (MaskEffortFile path) maskEffort = do
    let cfg = Y.setConfCompare (fieldOrder maskEffort) Y.defConfig
    let yaml = Y.encodePretty cfg maskEffort
    BS.writeFile path yaml

readCompMaskEffort :: CompInputFile -> IO CompMaskingEffort
readCompMaskEffort compFile = do
    putStrLn "Reading effort from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompMaskEffort
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show maskEffortFile
                readTaskMaskEffort maskEffortFile
            | maskEffortFile <- taskToMaskEffort <$> taskFiles
            ]

writeCompMaskEffort :: CompInputFile -> CompMaskingEffort -> IO ()
writeCompMaskEffort compFile compMaskEfforts = do
    putStrLn "Writing effort to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show maskEffortFile
            writeTaskMaskEffort maskEffortFile taskMaskEfforts

        | taskMaskEfforts <- unMkCompMaskEffort compMaskEfforts
        | maskEffortFile <- taskToMaskEffort <$> taskFiles
        ]
