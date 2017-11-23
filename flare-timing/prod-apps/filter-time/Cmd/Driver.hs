{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Cmd.Driver (driverMain) where

import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_, when, zipWithM)
import Control.Monad.Except (ExceptT, runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath.Find
    (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath
    ( FilePath
    , (</>), (<.>)
    , takeFileName, takeDirectory
    )
import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..))
import Cmd.Inputs (readTimeRowsFromCsv)
import Cmd.Outputs (writeTimeRowsToCsv)

import Flight.Comp (CompSettings(..), Pilot(..))
import Flight.TrackLog (IxTask(..), TrackFileFail)
import Flight.Units ()
import Flight.Mask (SigMasking, checkTracks)
import Flight.Track.Time (TimeRow(..), TickRow(..))

headers :: [String]
headers = ["tick", "distance"]

driverMain :: IO ()
driverMain = withCmdArgs drive

drive :: CmdOptions -> IO ()
drive CmdOptions{..} = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    dfe <- doesFileExist file
    if dfe then
        withFile file
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- find always (fileType ==? RegularFile &&? extension ==? ".comp-inputs.yaml") dir
            mapM_ withFile files
        else
            putStrLn "Couldn't find any flight score competition yaml input files."
    end <- getTime Monotonic
    fprint ("Filtering times completed in " % timeSpecs % "\n") start end
    where
        withFile compPath = do
            putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
            filterTime
                (IxTask <$> task)
                (Pilot <$> pilot)
                compPath
                checkAll

filterTime :: Show a
           => [IxTask]
           -> t2
           -> [Char]
           -> ([Char]
           -> [IxTask]
           -> t2
           -> ExceptT a IO [[Either (Pilot, t1) (Pilot, t)]])
           -> IO ()
filterTime selectTasks selectPilots compPath f = do
    checks <- runExceptT $ f compPath selectTasks selectPilots

    case checks of
        Left msg -> print msg
        Right xs -> do
            let ys :: [[Pilot]] =
                    (fmap . fmap)
                        (\case
                            Left (p, _) -> p
                            Right (p, _) -> p)
                        xs

            _ <- zipWithM
                (\ n zs ->
                    when (includeTask selectTasks $ IxTask n) $
                        mapM_ (readFilterWrite (takeDirectory compPath) n) zs)
                [1 .. ]
                ys

            return ()

checkAll :: FilePath
         -> [IxTask]
         -> [Pilot]
         -> ExceptT
             String
             IO
             [
                 [Either (Pilot, TrackFileFail) (Pilot, ())
                 ]
             ]
checkAll = checkTracks $ \CompSettings{tasks} -> filterCloser tasks

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)

fcsv :: FilePath -> Int -> Pilot -> (FilePath, FilePath)
fcsv dir task pilot =
    (d, f)
    where
        d = dir </> ".flare-timing" </> "align-time" </> "task-" ++ show task
        f = show pilot <.> "csv"

gcsv :: FilePath -> Int -> Pilot -> (FilePath, FilePath)
gcsv dir task pilot =
    (d, f)
    where
        d = dir </> ".flare-timing" </> "filter-time" </> "task-" ++ show task
        f = show pilot <.> "csv"

readFilterWrite :: FilePath -> Int -> Pilot -> IO ()
readFilterWrite dir iTask pilot = do
    _ <- createDirectoryIfMissing True dOut
    rows <- runExceptT $ readTimeRowsFromCsv (dIn </> f)
    case rows of
        Left msg -> print msg
        Right (_, rowsTime) -> do
            let rowsTick = timeToTick <$> rowsTime
            _ <- writeTimeRowsToCsv (dOut </> f) headers rowsTick
            return ()
    where
        (dIn, f) = fcsv dir iTask pilot
        (dOut, _) = gcsv dir iTask pilot

filterCloser :: SigMasking ()
filterCloser _ _ _ = ()

timeToTick :: TimeRow -> TickRow
timeToTick TimeRow{tick, distance} = TickRow tick distance
