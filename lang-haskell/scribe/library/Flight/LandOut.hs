module Flight.LandOut
    ( readAltLandOut, writeAltLandOut
    , readTaskLandOut, writeTaskLandOut
    , readCompLandOut, writeCompLandOut
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
    ( AltLandoutFile(..), CompInputFile(..), LandOutFile(..)
    , compFileToTaskFiles, taskToLandOut
    )

readAltLandOut :: (MonadThrow m, MonadIO m) => AltLandoutFile -> m CompLanding
readAltLandOut (AltLandoutFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeAltLandOut :: AltLandoutFile -> CompLanding -> IO ()
writeAltLandOut (AltLandoutFile path) landOut = do
    let cfg = Y.setConfCompare (fieldOrder landOut) Y.defConfig
    let yaml = Y.encodePretty cfg landOut
    BS.writeFile path yaml

readTaskLandOut :: (MonadThrow m, MonadIO m) => LandOutFile -> m TaskLanding
readTaskLandOut (LandOutFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskLandOut :: LandOutFile -> TaskLanding -> IO ()
writeTaskLandOut (LandOutFile path) landOut = do
    let cfg = Y.setConfCompare (fieldOrder landOut) Y.defConfig
    let yaml = Y.encodePretty cfg landOut
    BS.writeFile path yaml

readCompLandOut :: CompInputFile -> IO CompLanding
readCompLandOut compFile = do
    putStrLn "Reading land outs from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompLandOut
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show landOutFile
                readTaskLandOut landOutFile
            | landOutFile <- taskToLandOut <$> taskFiles
            ]

writeCompLandOut :: CompInputFile -> CompLanding -> IO ()
writeCompLandOut compFile compLandOuts = do
    putStrLn "Writing land outs to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show landOutFile
            writeTaskLandOut landOutFile taskLandOuts

        | taskLandOuts <- unMkCompLandOut compLandOuts
        | landOutFile <- taskToLandOut <$> taskFiles
        ]
