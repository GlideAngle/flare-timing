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
    ( TrackFileFail(..)
    , IxTask(..)
    , goalPilotTracks
    , filterPilots
    , filterTasks
    , makeAbsolute
    ) where

import Data.Bifunctor (bimap)
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

import qualified Data.Flight.Kml as K (Fix, parse)
import Data.Flight.Comp
    ( Pilot(..)
    , PilotTrackLogFile(..)
    , TrackLogFile(..)
    , TaskFolder(..)
    )

-- | 1-based indices of a task in a competition.
newtype IxTask = IxTask Int deriving (Eq, Show)

ixTasks :: [IxTask]
ixTasks = IxTask <$> [ 1 .. ]

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

goalPilotTrack :: ([K.Fix] -> a)
               -> PilotTrackLogFile
               -> ExceptT
                   (Pilot, TrackFileFail)
                   IO
                   (Pilot, a)
goalPilotTrack _ (PilotTrackLogFile p Nothing) =
    ExceptT . return $ Left (p, TrackLogFileNotSet)
goalPilotTrack f (PilotTrackLogFile p (Just (TrackLogFile file))) = do
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
                ExceptT . return $
                    bimap
                        (\msg -> (p, TrackLogFileNotRead msg))
                        (\fixes -> (p, f fixes))
                        kml

goalTaskPilotTracks :: (IxTask -> [K.Fix] -> a)
                    -> [ (IxTask, [ PilotTrackLogFile ]) ]
                    -> IO
                        [[ Either
                            (Pilot, TrackFileFail)
                            (Pilot, a)
                        ]]
goalTaskPilotTracks _ [] =
    return []
goalTaskPilotTracks f xs =
    sequence $ (\(i, pilotTracks) ->
        sequence $ (runExceptT . goalPilotTrack (f i)) <$> pilotTracks)
        <$> xs

goalPilotTracks :: (IxTask -> [K.Fix] -> a)
                -> [[ PilotTrackLogFile ]]
                -> IO
                    [[ Either
                        (Pilot, TrackFileFail)
                        (Pilot, a)
                    ]]
goalPilotTracks _ [] = return []
goalPilotTracks f tasks =
    goalTaskPilotTracks f (zip ixTasks tasks) 

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

filterTasks :: [ IxTask ]
            -> [[ PilotTrackLogFile ]]
            -> [[ PilotTrackLogFile ]]

filterTasks [] xs = xs
filterTasks tasks xs =
    zipWith (\i ys ->
        if i `elem` tasks then ys else []) ixTasks xs

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
