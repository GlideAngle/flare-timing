{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cmd.Driver (driverMain) where

import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_, when, zipWithM_)
import Control.Monad.Except (ExceptT, runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath.Find
    (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath
    ( FilePath
    , (</>), (<.>)
    , takeFileName, takeDirectory
    )
import Data.Vector (Vector)
import qualified Data.Vector as V (fromList, toList)

import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..))
import Cmd.Inputs (readTimeRowsFromCsv)
import Cmd.Outputs (writeTimeRowsToCsv)

import Flight.Comp (CompSettings(..), Pilot(..))
import Flight.TrackLog (IxTask(..), TrackFileFail)
import Flight.Units ()
import Flight.Mask (checkTracks)
import Flight.Track.Time (TimeRow(..), TickRow(..), discardFurther)

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

filterTime :: [IxTask]
           -> [Pilot]
           -> String
           -> (String
           -> [IxTask]
           -> [Pilot]
           -> ExceptT String IO [[Either (Pilot, _) (Pilot, _)]])
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

            _ <- zipWithM_
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
checkAll = checkTracks $ \CompSettings{tasks} -> (\ _ _ _ -> ()) tasks

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)

csvPathIn :: FilePath -> Int -> Pilot -> (FilePath, FilePath)
csvPathIn dir task pilot =
    (d, f)
    where
        d = dotDir "align-time" dir task
        f = show pilot <.> "csv"

csvDirOut :: FilePath -> Int -> FilePath
csvDirOut = dotDir "discard-further"

dotDir :: FilePath -> FilePath -> Int -> FilePath
dotDir name dir task =
    dir </> ".flare-timing" </> name </> "task-" ++ show task

readFilterWrite :: FilePath -> Int -> Pilot -> IO ()
readFilterWrite dir iTask pilot = do
    _ <- createDirectoryIfMissing True dOut
    rows <- runExceptT $ readTimeRowsFromCsv (dIn </> f)
    case rows of
        Left msg ->
            print msg

        Right (_, xs) ->
            writeTimeRowsToCsv (dOut </> f) headers $ discard xs
    where
        (dIn, f) = csvPathIn dir iTask pilot
        dOut = csvDirOut dir iTask

timeToTick :: TimeRow -> TickRow
timeToTick TimeRow{tick, distance} = TickRow tick distance

discard :: Vector TimeRow -> Vector TickRow
discard xs =
    V.fromList . discardFurther . dropZeros . V.toList $ timeToTick <$> xs

dropZeros :: [TickRow] -> [TickRow]
dropZeros =
    dropWhile ((== 0) . d)
    where
        d = distance :: (TickRow -> Double)
