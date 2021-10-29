module Flight.FarOut
    ( readTaskFarOut, writeTaskFarOut
    , readCompFarOut, writeCompFarOut
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y
import Control.Concurrent.ParallelIO (parallel, parallel_)

import Flight.Track.Land
    (TaskLanding(..), CompLanding(..), mkCompLandOut, unMkCompLandOut)
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    (CompInputFile(..), FarOutFile(..), compFileToTaskFiles, taskToFarOut)

readTaskFarOut :: (MonadThrow m, MonadIO m) => FarOutFile -> m TaskLanding
readTaskFarOut (FarOutFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskFarOut :: FarOutFile -> TaskLanding -> IO ()
writeTaskFarOut (FarOutFile path) landOut = do
    let cfg = Y.setConfCompare (fieldOrder landOut) Y.defConfig
    let yaml = Y.encodePretty cfg landOut
    BS.writeFile path yaml

readCompFarOut :: CompInputFile -> IO CompLanding
readCompFarOut compFile = do
    putStrLn "Reading far outs from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompLandOut
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show farOutFile
                readTaskFarOut farOutFile
            | farOutFile <- taskToFarOut <$> taskFiles
            ]

writeCompFarOut :: CompInputFile -> CompLanding -> IO ()
writeCompFarOut compFile compFarOuts = do
    putStrLn "Writing far outs to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show farOutFile
            writeTaskFarOut farOutFile taskFarOuts

        | taskFarOuts <- unMkCompLandOut compFarOuts
        | farOutFile <- taskToFarOut <$> taskFiles
        ]
