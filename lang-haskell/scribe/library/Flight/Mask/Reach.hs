module Flight.Mask.Reach
    ( readTaskMaskReach, writeTaskMaskReach
    , readCompMaskReach, writeCompMaskReach
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
    (CompInputFile(..), MaskReachFile(..), compFileToTaskFiles, taskToMaskReach)

readTaskMaskReach :: MonadIO m => MaskReachFile -> m TaskMaskingReach
readTaskMaskReach (MaskReachFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskMaskReach :: MaskReachFile -> TaskMaskingReach -> IO ()
writeTaskMaskReach (MaskReachFile path) maskReach = do
    let cfg = Y.setConfCompare (fieldOrder maskReach) Y.defConfig
    let yaml = Y.encodePretty cfg maskReach
    BS.writeFile path yaml

readCompMaskReach :: CompInputFile -> IO CompMaskingReach
readCompMaskReach compFile = do
    putStrLn "Reading reach from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompMaskReach
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show maskReachFile
                readTaskMaskReach maskReachFile
            | maskReachFile <- taskToMaskReach <$> taskFiles
            ]

writeCompMaskReach :: CompInputFile -> CompMaskingReach -> IO ()
writeCompMaskReach compFile compMaskReach = do
    putStrLn "Writing reach to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show maskReachFile
            writeTaskMaskReach maskReachFile taskMaskReachs

        | taskMaskReachs <- unMkCompMaskReach compMaskReach
        | maskReachFile <- taskToMaskReach <$> taskFiles
        ]
