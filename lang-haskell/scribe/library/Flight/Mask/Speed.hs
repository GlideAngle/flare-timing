module Flight.Mask.Speed
    ( readTaskMaskSpeed, writeTaskMaskSpeed
    , readCompMaskSpeed, writeCompMaskSpeed
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y
import Control.Concurrent.ParallelIO (parallel, parallel_)

import Flight.Track.Mask
    ( TaskMaskingSpeed(..), CompMaskingSpeed(..)
    , mkCompMaskSpeed, unMkCompMaskSpeed
    )
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    ( CompInputFile(..), MaskSpeedFile(..)
    , compFileToTaskFiles, taskToMaskSpeed
    )

readTaskMaskSpeed :: MonadIO m => MaskSpeedFile -> m TaskMaskingSpeed
readTaskMaskSpeed (MaskSpeedFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskMaskSpeed :: MaskSpeedFile -> TaskMaskingSpeed -> IO ()
writeTaskMaskSpeed (MaskSpeedFile path) maskSpeed = do
    let cfg = Y.setConfCompare (fieldOrder maskSpeed) Y.defConfig
    let yaml = Y.encodePretty cfg maskSpeed
    BS.writeFile path yaml

readCompMaskSpeed :: CompInputFile -> IO CompMaskingSpeed
readCompMaskSpeed compFile = do
    putStrLn "Reading speed from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompMaskSpeed
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show maskSpeedFile
                readTaskMaskSpeed maskSpeedFile
            | maskSpeedFile <- taskToMaskSpeed <$> taskFiles
            ]

writeCompMaskSpeed :: CompInputFile -> CompMaskingSpeed -> IO ()
writeCompMaskSpeed compFile compMaskSpeeds = do
    putStrLn "Writing speed to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show maskSpeedFile
            writeTaskMaskSpeed maskSpeedFile taskMaskSpeeds

        | taskMaskSpeeds <- unMkCompMaskSpeed compMaskSpeeds
        | maskSpeedFile <- taskToMaskSpeed <$> taskFiles
        ]
