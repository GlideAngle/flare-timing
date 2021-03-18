module Flight.CompInput
    ( readComp, writeComp
    , readTask, writeTask
    , readCompAndTasks, readCompAndTasksQuietly, writeCompAndTasks
    , readCompTracks, readCompTracksQuietly
    ) where

import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y
import Control.Concurrent.ParallelIO (parallel, parallel_)

import Flight.Field (FieldOrdering(..))
import Flight.Comp
    ( ScoringInputFiles, CompInputFile(..), TaskInputFile(..)
    , CompSettings(..), TaskSettings(..)
    , PilotTrackLogFile(..), TaskFolder(..)
    )

readComp :: (MonadThrow m, MonadIO m) => CompInputFile -> m (CompSettings k)
readComp (CompInputFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeComp :: CompInputFile -> CompSettings k -> IO ()
writeComp (CompInputFile path) compInput = do
    let cfg = Y.setConfCompare (fieldOrder compInput) Y.defConfig
    let yaml = Y.encodePretty cfg compInput
    BS.writeFile path yaml

readTask :: (MonadThrow m, MonadIO m) => TaskInputFile -> m (TaskSettings k)
readTask (TaskInputFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTask :: TaskInputFile -> TaskSettings k -> IO ()
writeTask (TaskInputFile path) taskInput = do
    let cfg = Y.setConfCompare (fieldOrder taskInput) Y.defConfig
    let yaml = Y.encodePretty cfg taskInput

    -- SEE: https://stackoverflow.com/questions/58682357/how-to-create-a-file-and-its-parent-directories-in-haskell
    createDirectoryIfMissing True $ takeDirectory path

    BS.writeFile path yaml

readCompTracksQuietly
    :: (MonadIO m, MonadThrow m)
    => ScoringInputFiles
    -> m ((CompSettings k, [TaskSettings k]), ([[PilotTrackLogFile]], [TaskFolder]))
readCompTracksQuietly files = do
    settings@(_, tss) <- readCompAndTasksQuietly files
    return (settings, ([pilots | TaskSettings{pilots} <- tss], taskFolder <$> tss))

readCompTracks
    :: ScoringInputFiles
    -> IO ((CompSettings k, [TaskSettings k]), ([[PilotTrackLogFile]], [TaskFolder]))
readCompTracks files = do
    settings@(_, tss) <- readCompAndTasks files
    return (settings, ([pilots | TaskSettings{pilots} <- tss], taskFolder <$> tss))

readCompAndTasksQuietly
    :: (MonadIO m, MonadThrow m)
    => ScoringInputFiles
    -> m (CompSettings k, [TaskSettings k])
readCompAndTasksQuietly (compFile, taskFiles) = do
    settingsComp <- readComp compFile
    settingsTasks <- sequence [readTask taskFile | taskFile <- taskFiles ]
    return (settingsComp, settingsTasks)

readCompAndTasks :: ScoringInputFiles -> IO (CompSettings k, [TaskSettings k])
readCompAndTasks (compFile, taskFiles) = do
    putStrLn $ "Reading comp inputs from " ++ show compFile
    settingsComp <- readComp compFile
    putStrLn "Reading task inputs from:"
    settingsTasks <-
        parallel
            [ do
                putStrLn $ "\t" ++ show taskFile
                readTask taskFile
            | taskFile <- taskFiles
          ]
    return (settingsComp, settingsTasks)

writeCompAndTasks :: ScoringInputFiles -> (CompSettings k, [TaskSettings k]) -> IO ()
writeCompAndTasks (compFile, taskFiles) (comp, tasks) = do
    putStrLn $ "Writing comp inputs to " ++ show compFile
    writeComp compFile comp
    putStrLn "Writing task inputs to:"
    parallel_
        [ do
            putStrLn $ "\t" ++ show taskFile
            writeTask taskFile taskSetting

        | taskSetting <- tasks
        | taskFile <- taskFiles
        ]
