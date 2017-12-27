{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ParallelListComp #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cmd.Driver (driverMain) where

import Data.Maybe (listToMaybe)
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import Data.Time.Clock (diffUTCTime)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (join, mapM_, when)
import Control.Monad.Except (ExceptT, runExceptT)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)

import Flight.Cmd.Paths (checkPaths)
import Flight.Cmd.Options (CmdOptions(..), ProgramName(..), mkOptions)
import Cmd.Options (description)

import Flight.Comp
    ( DiscardDir(..)
    , AlignDir(..)
    , CompInputFile(..)
    , TagZoneFile(..)
    , TaskLengthFile(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , CompSettings(..)
    , Task(..)
    , OpenClose(..)
    , Pilot(..)
    , TrackFileFail
    , IxTask(..)
    , StartEnd
    , SpeedSection
    , compFileToCompDir
    , compToTaskLength
    , compToCross
    , crossToTag
    , discardDir
    , alignPath
    , findCompInput
    )
import Flight.Track.Time (taskToLeading, discard)
import Flight.Track.Mask (RaceTime(..))
import Flight.Units ()
import Flight.Mask (checkTracks)
import Flight.Scribe
    (readComp, readRoute, readTagging, readAlignTime, writeDiscardFurther)
import Flight.Lookup.Route (RouteLookup(..), routeLength)
import Flight.Lookup.Tag (TaskTimeLookup(..), tagTaskTime)
import Data.Aeson.ViaScientific (ViaScientific(..))
import Flight.Score (EssTime(..), TaskDeadline(..))

headers :: [String]
headers = ["tick", "distance", "areaStep"]

driverMain :: IO ()
driverMain = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing
    err <- checkPaths options
    maybe (drive options) putStrLn err

drive :: CmdOptions -> IO ()
drive o = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findCompInput o
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    fprint ("Filtering times completed in " % timeSpecs % "\n") start end

go :: CmdOptions -> CompInputFile -> IO ()
go CmdOptions{..} compFile@(CompInputFile compPath) = do
    let lenFile@(TaskLengthFile lenPath) = compToTaskLength $ compFile
    let tagFile@(TagZoneFile tagPath) = crossToTag . compToCross $ compFile
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading task length from '" ++ takeFileName lenPath ++ "'"
    putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"

    compSettings <- runExceptT $ readComp compFile
    tagging <- runExceptT $ readTagging tagFile
    routes <- runExceptT $ readRoute lenFile

    case (compSettings, tagging, routes) of
        (Left msg, _, _) -> putStrLn msg
        (_, Left msg, _) -> putStrLn msg
        (_, _, Left msg) -> putStrLn msg
        (Right cs, Right _, Right _) ->
            filterTime
                cs
                (routeLength routes)
                (tagTaskTime tagging)
                compFile
                (IxTask <$> task)
                (Pilot <$> pilot)
                checkAll

filterTime
    :: CompSettings
    -> RouteLookup
    -> TaskTimeLookup
    -> CompInputFile
    -> [IxTask]
    -> [Pilot]
    -> (CompInputFile
        -> [IxTask]
        -> [Pilot]
        -> ExceptT String IO [[Either (Pilot, _) (Pilot, _)]])
    -> IO ()
filterTime
    CompSettings{tasks}
    lengths
    (TaskTimeLookup lookupTaskTime)
    compFile selectTasks selectPilots f = do

    checks <- runExceptT $ f compFile selectTasks selectPilots

    case checks of
        Left msg -> print msg
        Right xs -> do
            let taskPilots :: [[Pilot]] =
                    (fmap . fmap)
                        (\case
                            Left (p, _) -> p
                            Right (p, _) -> p)
                        xs

            let iTasks = IxTask <$> [1 .. length taskPilots]

            let raceStartEnd :: [Maybe StartEnd] =
                    join <$>
                    [ ($ s) . ($ i) <$> lookupTaskTime
                    | i <- iTasks
                    | s <- speedSection <$> tasks
                    ]

            let raceTime :: [Maybe RaceTime] =
                    join <$>
                    [ racing (openClose ss zt) <$> se
                    | ss <- speedSection <$> tasks
                    | zt <- zoneTimes <$> tasks
                    | se <- raceStartEnd
                    ]

            _ <- sequence $ zipWith3
                (\ n rt pilots ->
                        mapM_
                        (readFilterWrite
                            lengths
                            compFile
                            (includeTask selectTasks)
                            n
                            rt)
                        pilots)
                (IxTask <$> [1 .. ])
                raceTime
                taskPilots

            return ()

checkAll :: CompInputFile
         -> [IxTask]
         -> [Pilot]
         -> ExceptT
             String
             IO
             [
                 [Either (Pilot, TrackFileFail) (Pilot, ())
                 ]
             ]
checkAll = checkTracks $ \CompSettings{tasks} -> (\ _ _ _ -> ()) tasks

openClose :: SpeedSection -> [OpenClose] -> Maybe OpenClose
openClose _ [] = Nothing
openClose Nothing (x : _) = Just x
openClose _ [x] = Just x
openClose (Just (_, e)) xs = listToMaybe . take 1 . drop (e - 1) $ xs

racing :: Maybe OpenClose -> StartEnd -> Maybe RaceTime
racing oc (s, e) = do
    OpenClose{open, close} <- oc
    return
        RaceTime
            { openTask = open
            , closeTask = close
            , firstStart = s
            , lastArrival = e
            , tickArrival =
                ViaScientific . EssTime
                <$> (\e' -> toRational $ e' `diffUTCTime` s) <$> e
            , tickRace =
                ViaScientific . EssTime . toRational
                $ close `diffUTCTime` s
            , tickTask =
                ViaScientific . EssTime . toRational
                $ close `diffUTCTime` open 
            }

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)

readFilterWrite
    :: RouteLookup
    -> CompInputFile
    -> (IxTask -> Bool)
    -> IxTask
    -> Maybe RaceTime
    -> Pilot
    -> IO ()
readFilterWrite
    (RouteLookup lookupTaskLength)
    compFile
    selectTask
    iTask@(IxTask i) raceTime pilot =
    when (selectTask iTask) $ do
    _ <- createDirectoryIfMissing True dOut
    rows <- runExceptT $ readAlignTime (AlignTimeFile (dIn </> file))
    either print (f . discard deadline dRace . snd) rows
    where
        f = writeDiscardFurther (DiscardFurtherFile $ dOut </> file) headers
        dir = compFileToCompDir compFile
        (AlignDir dIn, AlignTimeFile file) = alignPath dir i pilot
        (DiscardDir dOut) = discardDir dir i
        taskLength = join (($ iTask) <$> lookupTaskLength)
        dRace = taskToLeading <$> taskLength
        deadline =
            TaskDeadline
            . (\RaceTime{tickRace = ViaScientific (EssTime tRace)} -> tRace)
            <$> raceTime

