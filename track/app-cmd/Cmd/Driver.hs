{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cmd.Driver (driverMain) where

import Data.Maybe (catMaybes)
import Control.Monad (mapM_)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath
    ( FilePath
    , (</>)
    , takeFileName
    , takeDirectory
    , normalise
    , splitDirectories
    , joinPath
    )

import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..), Reckon(..))
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)

import qualified Data.Flight.Kml as K (parse)
import Data.Flight.Comp
    ( CompSettings(..)
    , Pilot(..)
    , PilotTrackLogFile(..)
    , TrackLogFile(..)
    , TaskFolder(..)
    )

type Task = Int

driverMain :: IO ()
driverMain = withCmdArgs drive

drive :: CmdOptions -> IO ()
drive CmdOptions{..} = do
    dfe <- doesFileExist file
    if dfe then
        go file
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- find always (fileType ==? RegularFile &&? extension ==? ".comp.yaml") dir
            mapM_ go files
        else
            putStrLn "Couldn't find any flight score competition yaml input files."
    where
        go yamlCompPath = do
            putStrLn $ takeFileName yamlCompPath

            case reckon of
                Goal -> do
                    made <- runExceptT $ printMadeGoal
                                            yamlCompPath
                                            task
                                            (Pilot <$> pilot)
                    case made of
                        Left msg -> print msg
                        Right tracks -> print tracks

                x ->
                    putStrLn $ "TODO: Handle other reckon of " ++ show x

data PilotTrackStatus
    = TaskFolderExistsNot String
    | TrackLogFileExistsNot String
    | TrackLogFileNotSet
    | TrackLogFileRead Int
    | TrackLogFileNotRead String

instance Show PilotTrackStatus where
    show (TaskFolderExistsNot x) = "Folder '" ++ x ++ "' not found"
    show (TrackLogFileExistsNot x) = "File '" ++ x ++ "' not found"
    show TrackLogFileNotSet = "File not set"
    show (TrackLogFileNotRead "") = "File not read"
    show (TrackLogFileNotRead x) = "File not read " ++ x
    show (TrackLogFileRead count) = show count ++ " fixes"

goalPilotTrack :: PilotTrackLogFile -> IO (Pilot, PilotTrackStatus)
goalPilotTrack (PilotTrackLogFile p Nothing) = return (p, TrackLogFileNotSet)
goalPilotTrack (PilotTrackLogFile p (Just (TrackLogFile file))) = do
    let folder = takeDirectory file
    dde <- doesDirectoryExist folder
    if not dde then return (p, TaskFolderExistsNot folder) else do
        dfe <- doesFileExist file
        if not dfe then return (p, TrackLogFileExistsNot file) else do
            contents <- readFile file
            kml <- K.parse contents
            case kml of
                Left msg -> return (p, TrackLogFileNotRead msg)
                Right fixes -> return (p, TrackLogFileRead $ length fixes)

goalTaskPilotTracks :: [ (Int, [ PilotTrackLogFile ]) ]
                    -> IO [[(Pilot, PilotTrackStatus)]]
goalTaskPilotTracks [] = return []
goalTaskPilotTracks xs = do
    zs <- sequence $ (\(_, pilotTracks) -> do
                ys <- sequence $ goalPilotTrack <$> pilotTracks
                return ys)
                <$> xs
    return $ zs

goalPilotTracks :: [[ PilotTrackLogFile ]] -> IO [[(Pilot, PilotTrackStatus)]]
goalPilotTracks [] = return []
goalPilotTracks tasks = do
    xs <- goalTaskPilotTracks (zip [ 1 .. ] tasks) 
    return xs

filterPilots :: [ Pilot ]
             -> [[ PilotTrackLogFile ]]
             -> [[ PilotTrackLogFile ]]

filterPilots [] xs = xs
filterPilots pilots xs =
    f <$> xs
    where
        f :: [ PilotTrackLogFile ] -> [ PilotTrackLogFile ]
        f ys =
            catMaybes
            $ (\x@(PilotTrackLogFile pilot _) ->
                if pilot `elem` pilots then Just x else Nothing)
            <$> ys

filterTasks :: [ Task ]
            -> [[ PilotTrackLogFile ]]
            -> [[ PilotTrackLogFile ]]

filterTasks [] xs = xs
filterTasks tasks xs =
    zipWith (\i ys ->
        if i `elem` tasks then ys else []) [ 1 .. ] xs

makeAbsolute :: FilePath -> TaskFolder -> PilotTrackLogFile -> PilotTrackLogFile
makeAbsolute _ _ x@(PilotTrackLogFile _ Nothing) = x
makeAbsolute dir (TaskFolder pathParts) (PilotTrackLogFile p (Just (TrackLogFile file))) =
    PilotTrackLogFile p (Just (TrackLogFile path))
    where
        parts :: [ FilePath ]
        parts = splitDirectories dir ++ pathParts

        path :: FilePath
        path = normalise $ (joinPath parts) </> file

readSettings :: FilePath -> ExceptT String IO CompSettings
readSettings compYamlPath = do
    contents <- lift $ BS.readFile compYamlPath
    ExceptT . return $ decodeEither contents

printMadeGoal :: FilePath
              -> [Task]
              -> [Pilot]
              -> ExceptT String IO [[(Pilot, PilotTrackStatus)]]
printMadeGoal compYamlPath tasks selectPilots = do
    (CompSettings {pilots, taskFolders}) <- readSettings compYamlPath
    let dir = takeDirectory compYamlPath
    let ys = filterPilots selectPilots $ filterTasks tasks pilots
    let fs = (makeAbsolute dir) <$> taskFolders
    let zs = zipWith (\f y -> f <$> y) fs ys
    lift $ goalPilotTracks zs
