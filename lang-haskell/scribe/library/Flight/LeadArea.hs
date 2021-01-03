module Flight.LeadArea
    ( readTaskLeadArea, writeTaskLeadArea
    , readCompLeadArea, writeCompLeadArea
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y
import Control.Concurrent.ParallelIO (parallel, parallel_)
import Data.UnitsOfMeasure (KnownUnit, Unpack)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Track.Lead
    (TaskLeading(..), CompLeading(..), mkCompLeadArea, unMkCompLeadArea)
import Flight.Field (FieldOrdering(..))
import Flight.Comp
    (CompInputFile(..), LeadAreaFile(..), compFileToTaskFiles, taskToLeadArea)

readTaskLeadArea
    :: (MonadIO m, FromJSON (TaskLeading q))
    => LeadAreaFile
    -> m (TaskLeading q)
readTaskLeadArea (LeadAreaFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTaskLeadArea
    :: (ToJSON (TaskLeading q))
    => LeadAreaFile
    -> TaskLeading q
    -> IO ()
writeTaskLeadArea (LeadAreaFile path) leading = do
    let cfg = Y.setConfCompare (fieldOrder leading) Y.defConfig
    let yaml = Y.encodePretty cfg leading
    BS.writeFile path yaml

readCompLeadArea
    :: (KnownUnit (Unpack u), q ~ Quantity Double u)
    => CompInputFile
    -> IO (CompLeading q)
readCompLeadArea compFile = do
    putStrLn "Reading leading areas from:"
    taskFiles <- compFileToTaskFiles compFile
    mkCompLeadArea
        <$> parallel
            [ do
                putStrLn $ "\t" ++ show leadAreaFile
                readTaskLeadArea leadAreaFile
            | leadAreaFile <- taskToLeadArea <$> taskFiles
            ]

writeCompLeadArea
    :: (KnownUnit (Unpack u), q ~ Quantity Double u)
    => CompInputFile
    -> CompLeading q
    -> IO ()
writeCompLeadArea compFile compLeadAreas = do
    putStrLn "Writing leading areas to:"
    taskFiles <- compFileToTaskFiles compFile
    parallel_
        [ do
            putStrLn $ "\t" ++ show leadAreaFile
            writeTaskLeadArea leadAreaFile taskLeadAreas

        | taskLeadAreas <- unMkCompLeadArea compLeadAreas
        | leadAreaFile <- taskToLeadArea <$> taskFiles
        ]
