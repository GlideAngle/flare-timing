module Flight.Mask.Arrival
    ( readTaskMaskArrival, writeTaskMaskArrival
    , readCompMaskArrival, writeCompMaskArrival
    , readAltArrival, writeAltArrival
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

import Flight.Track.Mask
    ( TaskMaskingArrival(..), CompMaskingArrival(..)
    , mkCompMaskArrival, unMkCompMaskArrival
    )
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    ( CompInputFile(..), MaskArrivalFile(..), AltArrivalFile(..)
    , compFileToTaskFiles, taskToMaskArrival
    )

readAltArrival :: (MonadThrow m, MonadIO m) => AltArrivalFile -> m CompMaskingArrival
readAltArrival (AltArrivalFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeAltArrival :: AltArrivalFile -> CompMaskingArrival -> IO ()
writeAltArrival (AltArrivalFile path) altArrival = do
    let cfg = Y.setConfCompare (fieldOrder altArrival) Y.defConfig
    let yaml = Y.encodePretty cfg altArrival

    createDirectoryIfMissing True $ takeDirectory path
    BS.writeFile path yaml

readTaskMaskArrival :: MonadIO m => MaskArrivalFile -> m TaskMaskingArrival
readTaskMaskArrival (MaskArrivalFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskMaskArrival :: MaskArrivalFile -> TaskMaskingArrival -> IO ()
writeTaskMaskArrival (MaskArrivalFile path) maskArrival = do
    let cfg = Y.setConfCompare (fieldOrder maskArrival) Y.defConfig
    let yaml = Y.encodePretty cfg maskArrival
    BS.writeFile path yaml

readCompMaskArrival :: CompInputFile -> IO CompMaskingArrival
readCompMaskArrival compFile = do
    putStrLn "Reading arrivals from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompMaskArrival
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show maskArrivalFile
                readTaskMaskArrival maskArrivalFile
            | maskArrivalFile <- taskToMaskArrival <$> taskFiles
            ]

writeCompMaskArrival :: CompInputFile -> CompMaskingArrival -> IO ()
writeCompMaskArrival compFile compMaskArrivals = do
    putStrLn "Writing arrivals to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show maskArrivalFile
            writeTaskMaskArrival maskArrivalFile taskMaskArrivals

        | taskMaskArrivals <- unMkCompMaskArrival compMaskArrivals
        | maskArrivalFile <- taskToMaskArrival <$> taskFiles
        ]
