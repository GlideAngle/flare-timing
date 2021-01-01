module Flight.CompInput
    ( readComp, writeComp
    , readTask, writeTask
    , readCompAndTasks
    , readCompTracks
    , compFileToTaskFiles
    ) where

import Prelude hiding (readFile, writeFile)
import System.FilePath (takeDirectory)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y

import Flight.Field (FieldOrdering(..))
import Flight.Comp
    ( ScoringInputFiles, CompInputFile(..), TaskInputFile(..)
    , CompSettings(..), TaskSettings(..)
    , PilotTrackLogFile(..), TaskFolder(..)
    , FindDirFile(..), FileType(TaskInput)
    , findTaskInput, reshape
    )

readComp
    :: (MonadThrow m, MonadIO m)
    => CompInputFile
    -> m (CompSettings k)
readComp (CompInputFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeComp :: CompInputFile -> CompSettings k -> IO ()
writeComp (CompInputFile path) compInput = do
    let cfg = Y.setConfCompare (fieldOrder compInput) Y.defConfig
    let yaml = Y.encodePretty cfg compInput
    BS.writeFile path yaml

readTask
    :: (MonadThrow m, MonadIO m)
    => TaskInputFile
    -> m (TaskSettings k)
readTask (TaskInputFile path) = do
    contents <- liftIO $ BS.readFile path
    decodeThrow contents

writeTask :: TaskInputFile -> TaskSettings k -> IO ()
writeTask (TaskInputFile path) taskInput = do
    let cfg = Y.setConfCompare (fieldOrder taskInput) Y.defConfig
    let yaml = Y.encodePretty cfg taskInput
    BS.writeFile path yaml

readCompTracks
    :: (MonadThrow m, MonadIO m)
    => ScoringInputFiles
    -> m ((CompSettings k, [TaskSettings k]), ([[PilotTrackLogFile]], [TaskFolder]))
readCompTracks files = do
    settings@(_, tss) <- readCompAndTasks files
    return (settings, ([pilots | TaskSettings{pilots} <- tss], taskFolder <$> tss))

readCompAndTasks
    :: (MonadIO m, MonadThrow m)
    => ScoringInputFiles
    -> m (CompSettings k, [TaskSettings k])
readCompAndTasks (compFile, taskFiles) = do
    settingsComp <- readComp compFile
    settingsTasks <- sequence $ readTask <$> taskFiles
    return (settingsComp, settingsTasks)

compFileToTaskFiles :: CompInputFile -> IO [TaskInputFile]
compFileToTaskFiles (CompInputFile pathComp) = do
    let pathTask = reshape TaskInput pathComp
    files <- findTaskInput $ FindDirFile {dir = takeDirectory pathComp, file = pathTask}
    return files
