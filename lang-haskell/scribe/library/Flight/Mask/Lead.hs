module Flight.Mask.Lead
    ( readTaskMaskLead, writeTaskMaskLead
    , readCompMaskLead, writeCompMaskLead
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y
import Control.Concurrent.ParallelIO (parallel, parallel_)
import Data.UnitsOfMeasure (KnownUnit, Unpack)

import Flight.Track.Mask
    ( TaskMaskingLead(..), CompMaskingLead(..)
    , mkCompMaskLead, unMkCompMaskLead
    )
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    ( CompInputFile(..), MaskLeadFile(..)
    , compFileToTaskFiles, taskToMaskLead
    )

readTaskMaskLead
    :: (KnownUnit (Unpack u), KnownUnit (Unpack v), MonadIO m)
    => MaskLeadFile
    -> m (TaskMaskingLead u v)
readTaskMaskLead (MaskLeadFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskMaskLead
    :: (KnownUnit (Unpack u), KnownUnit (Unpack v))
    => MaskLeadFile
    -> TaskMaskingLead u v
    -> IO ()
writeTaskMaskLead (MaskLeadFile path) maskLead = do
    let cfg = Y.setConfCompare (fieldOrder maskLead) Y.defConfig
    let yaml = Y.encodePretty cfg maskLead
    BS.writeFile path yaml

readCompMaskLead
    :: (KnownUnit (Unpack u), KnownUnit (Unpack v))
    => CompInputFile
    -> IO (CompMaskingLead u v)
readCompMaskLead compFile = do
    putStrLn "Reading leading from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompMaskLead
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show maskLeadFile
                readTaskMaskLead maskLeadFile
            | maskLeadFile <- taskToMaskLead <$> taskFiles
            ]

writeCompMaskLead
    :: (KnownUnit (Unpack u), KnownUnit (Unpack v))
    => CompInputFile
    -> (CompMaskingLead u v)
    -> IO ()
writeCompMaskLead compFile compMaskLeads = do
    putStrLn "Writing leading to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show maskLeadFile
            writeTaskMaskLead maskLeadFile taskMaskLeads

        | taskMaskLeads <- unMkCompMaskLead compMaskLeads
        | maskLeadFile <- taskToMaskLead <$> taskFiles
        ]
