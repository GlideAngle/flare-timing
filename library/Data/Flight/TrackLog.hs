{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Data.Flight.TrackLog
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Competition pilot tracks logs.
-}
module Data.Flight.TrackLog
    ( PilotTrackFixes(..)
    , TrackFileFail(..)
    , Task
    , goalPilotTracks
    , filterPilots
    , filterTasks
    , makeAbsolute
    ) where

import Data.Maybe (catMaybes)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath
    ( FilePath
    , (</>)
    , takeDirectory
    , normalise
    , splitDirectories
    , joinPath
    )

import qualified Data.Flight.Kml as K (parse)
import Data.Flight.Comp
    ( Pilot(..)
    , PilotTrackLogFile(..)
    , TrackLogFile(..)
    , TaskFolder(..)
    )

type Task = Int

newtype PilotTrackFixes = PilotTrackFixes Int deriving Show

data TrackFileFail
    = TaskFolderExistsNot String
    | TrackLogFileExistsNot String
    | TrackLogFileNotSet
    | TrackLogFileNotRead String

instance Show TrackFileFail where
    show (TaskFolderExistsNot x) = "Folder '" ++ x ++ "' not found"
    show (TrackLogFileExistsNot x) = "File '" ++ x ++ "' not found"
    show TrackLogFileNotSet = "File not set"
    show (TrackLogFileNotRead "") = "File not read"
    show (TrackLogFileNotRead x) = "File not read " ++ x

goalPilotTrack :: PilotTrackLogFile
               -> ExceptT
                   (Pilot, TrackFileFail)
                   IO
                   (Pilot, PilotTrackFixes)
goalPilotTrack (PilotTrackLogFile p Nothing) =
    ExceptT . return $ Left (p, TrackLogFileNotSet)
goalPilotTrack (PilotTrackLogFile p (Just (TrackLogFile file))) = do
    let folder = takeDirectory file
    dde <- lift $ doesDirectoryExist folder
    if not dde
        then
            ExceptT . return $
                Left (p, TaskFolderExistsNot folder)
        else do
            dfe <- lift $ doesFileExist file
            if not dfe
                then
                    ExceptT . return $
                        Left (p, TrackLogFileExistsNot file)
            else do
                contents <- lift $ readFile file
                kml <- lift $ K.parse contents
                case kml of
                    Left msg ->
                        ExceptT . return $
                            Left (p, TrackLogFileNotRead msg)

                    Right fixes ->
                        ExceptT . return $
                            Right (p, PilotTrackFixes $ length fixes)

goalTaskPilotTracks :: [ (Int, [ PilotTrackLogFile ]) ]
                    -> IO
                        [[ Either
                            (Pilot, TrackFileFail)
                            (Pilot, PilotTrackFixes)
                        ]]
goalTaskPilotTracks [] =
    return []
goalTaskPilotTracks xs =
    sequence $ (\(_, pilotTracks) ->
        sequence $ (runExceptT . goalPilotTrack) <$> pilotTracks)
        <$> xs

goalPilotTracks :: [[ PilotTrackLogFile ]]
                -> IO
                    [[ Either
                        (Pilot, TrackFileFail)
                        (Pilot, PilotTrackFixes)
                    ]]
goalPilotTracks [] = return []
goalPilotTracks tasks =
    goalTaskPilotTracks (zip [ 1 .. ] tasks) 

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

makeAbsolute :: FilePath
             -> TaskFolder
             -> PilotTrackLogFile
             -> PilotTrackLogFile
makeAbsolute _ _ x@(PilotTrackLogFile _ Nothing) = x
makeAbsolute
    dir
    (TaskFolder pathParts)
    (PilotTrackLogFile p (Just (TrackLogFile file))) =
    PilotTrackLogFile p (Just (TrackLogFile path))
    where
        parts :: [ FilePath ]
        parts = splitDirectories dir ++ pathParts

        path :: FilePath
        path = normalise $ joinPath parts </> file
