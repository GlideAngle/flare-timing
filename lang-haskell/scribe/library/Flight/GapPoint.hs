module Flight.GapPoint
    ( readAltScore, writeAltScore
    , readTaskGapPoint, writeTaskGapPoint
    , readCompGapPoint, writeCompGapPoint
    ) where

import Prelude hiding (readFile, writeFile)
import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y
import Control.Concurrent.ParallelIO (parallel, parallel_)

import Flight.Track.Point
    ( AltPointing(..), TaskPointing(..), CompPointing(..)
    , mkCompGapPoint, unMkCompGapPoint
    )
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    ( AltScoreFile(..), CompInputFile(..), GapPointFile(..)
    , compFileToTaskFiles, taskToGapPoint
    )

readAltScore :: (MonadThrow m, MonadIO m) => AltScoreFile -> m AltPointing
readAltScore (AltScoreFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeAltScore :: AltScoreFile -> AltPointing -> IO ()
writeAltScore (AltScoreFile path) altPointing = do
    let cfg = Y.setConfCompare (fieldOrder altPointing) Y.defConfig
    let yaml = Y.encodePretty cfg altPointing

    createDirectoryIfMissing True $ takeDirectory path
    BS.writeFile path yaml

readTaskGapPoint :: (MonadThrow m, MonadIO m) => GapPointFile -> m TaskPointing
readTaskGapPoint (GapPointFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskGapPoint :: GapPointFile -> TaskPointing -> IO ()
writeTaskGapPoint (GapPointFile path) gapPoint = do
    let cfg = Y.setConfCompare (fieldOrder gapPoint) Y.defConfig
    let yaml = Y.encodePretty cfg gapPoint
    BS.writeFile path yaml

readCompGapPoint :: CompInputFile -> IO CompPointing
readCompGapPoint compFile = do
    putStrLn "Reading GAP points from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompGapPoint
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show gapPointFile
                readTaskGapPoint gapPointFile
            | gapPointFile <- taskToGapPoint <$> taskFiles
            ]

writeCompGapPoint :: CompInputFile -> CompPointing -> IO ()
writeCompGapPoint compFile compGapPoints = do
    putStrLn "Writing GAP points to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show gapPointFile
            writeTaskGapPoint gapPointFile taskGapPoints

        | taskGapPoints <- unMkCompGapPoint compGapPoints
        | gapPointFile <- taskToGapPoint <$> taskFiles
        ]
