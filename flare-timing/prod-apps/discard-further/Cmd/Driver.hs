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

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cmd.Driver (driverMain) where

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (join, mapM_, when, zipWithM_)
import Control.Monad.Except (ExceptT, runExceptT)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)
import Data.Vector (Vector)
import qualified Data.Vector as V (fromList, toList)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Cmd.Paths (checkPaths)
import Flight.Cmd.Options (CmdOptions(..), ProgramName(..), mkOptions)
import Cmd.Options (description)

import Flight.Comp
    ( DiscardDir(..)
    , AlignDir(..)
    , CompInputFile(..)
    , TaskLengthFile(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , CompSettings(..)
    , Pilot(..)
    , TrackFileFail
    , IxTask(..)
    , compToTaskLength
    , compFileToCompDir
    , discardDir
    , alignPath
    , findCompInput
    )
import Flight.Units ()
import Flight.Mask (checkTracks)
import Flight.Track.Time
    (TimeRow(..), TickRow(..), LeadingDistance(..), discardFurther, leadingArea)
import Flight.Scribe (readRoute, readAlignTime, writeDiscardFurther)
import Flight.Distance (TaskDistance(..))
import Flight.Lookup.Route (RouteLookup(..), routeLength)
import Flight.Score (LeadingAreaStep(..))
import Data.Aeson.ViaScientific (ViaScientific(..))

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
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading task length from '" ++ takeFileName lenPath ++ "'"

    routes <- runExceptT $ readRoute lenFile

    filterTime
        (routeLength routes)
        compFile
        (IxTask <$> task)
        (Pilot <$> pilot)
        checkAll

filterTime :: RouteLookup
           -> CompInputFile
           -> [IxTask]
           -> [Pilot]
           -> (CompInputFile
               -> [IxTask]
               -> [Pilot]
               -> ExceptT String IO [[Either (Pilot, _) (Pilot, _)]])
           -> IO ()
filterTime lengths compFile selectTasks selectPilots f = do
    checks <- runExceptT $ f compFile selectTasks selectPilots

    case checks of
        Left msg -> print msg
        Right xs -> do
            let ys :: [[Pilot]] =
                    (fmap . fmap)
                        (\case
                            Left (p, _) -> p
                            Right (p, _) -> p)
                        xs

            _ <- zipWithM_
                (\ n zs ->
                    when (includeTask selectTasks n) $
                        mapM_ (readFilterWrite lengths compFile n) zs)
                (IxTask <$> [1 .. ])
                ys

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

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)

readFilterWrite :: RouteLookup -> CompInputFile -> IxTask -> Pilot -> IO ()
readFilterWrite
    (RouteLookup lookupTaskLength)
    compFile iTask@(IxTask i) pilot = do
    _ <- createDirectoryIfMissing True dOut
    rows <- runExceptT $ readAlignTime (AlignTimeFile (dIn </> file))
    either print (f . discard leadingDistance . snd) rows
    where
        f = writeDiscardFurther (DiscardFurtherFile $ dOut </> file) headers
        dir = compFileToCompDir compFile
        (AlignDir dIn, AlignTimeFile file) = alignPath dir i pilot
        (DiscardDir dOut) = discardDir dir i
        taskLength = join (($ iTask) <$> lookupTaskLength)
        leadingDistance = taskToLeading <$> taskLength

taskToLeading :: TaskDistance Double -> LeadingDistance
taskToLeading (TaskDistance d) =
    LeadingDistance $ d'
    where
        d' = convert d :: Quantity Double [u| km |]

timeToTick :: TimeRow -> TickRow
timeToTick TimeRow{tick, distance} =
    TickRow tick distance $ ViaScientific (LeadingAreaStep 0)

discard :: Maybe LeadingDistance -> Vector TimeRow -> Vector TickRow
discard d xs =
    V.fromList
    . leadingArea d
    . discardFurther
    . dropZeros
    . V.toList
    $ timeToTick <$> xs

dropZeros :: [TickRow] -> [TickRow]
dropZeros =
    dropWhile ((== 0) . d)
    where
        d = distance :: (TickRow -> Double)
