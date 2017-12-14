{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cmd.Driver (driverMain) where

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.String (IsString)
import Control.Monad (mapM_)
import Control.Monad.Except (runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (FilePath, takeFileName)
import Flight.Cmd.Paths (checkPaths)
import Cmd.Options (CmdOptions(..), mkOptions)
import Cmd.Settings (readCompSettings)
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS

import Flight.Units ()
import Flight.Comp
    ( CompSettings(tasks), Task(zones), CompInputFile(..), TaskLengthFile(..)
    , compToTaskLength
    )
import Flight.TaskTrack.Rational (taskTracks)
import qualified Flight.TaskTrack as TZ
    (TaskTrack(..), TaskRoutes(..))

driverMain :: IO ()
driverMain = do
    name <- getProgName
    options <- cmdArgs $ mkOptions name
    err <- checkPaths options
    case err of
        Just msg -> putStrLn msg
        Nothing -> drive options

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        ("easting", _) -> LT
        ("northing", _) -> GT
        ("latZone", _) -> LT
        ("lngZone", _) -> GT
        ("pointToPoint", _) -> LT
        ("projection", "pointToPoint") -> GT
        ("projection", _) -> LT
        ("edgeToEdge", _) -> GT
        ("lat", _) -> LT
        ("lng", _) -> GT
        ("distance", _) -> LT
        ("legs", "distance") -> GT
        ("legs", _) -> LT
        ("legsSum", "distance") -> GT
        ("legsSum", "legs") -> GT
        ("legsSum", _) -> LT
        ("wayPoints", _) -> GT
        ("mappedPoints", "distance") -> GT
        ("mappedPoints", "legs") -> GT
        ("mappedPoints", "legsSum") -> GT
        ("mappedPoints", _) -> LT
        ("mappedZones", _) -> GT
        _ -> compare a b

drive :: CmdOptions -> IO ()
drive CmdOptions{..} = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    dfe <- doesFileExist file
    if dfe then
        withFile $ CompInputFile file
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- find always (fileType ==? RegularFile &&? extension ==? ".comp-input.yaml") dir
            mapM_ withFile (CompInputFile <$> files)
        else
            putStrLn "Couldn't find any flight score competition yaml input files."
    end <- getTime Monotonic
    fprint ("Measuring task lengths completed in " % timeSpecs % "\n") start end
    where
        withFile compFile@(CompInputFile compPath) = do
            putStrLn $ takeFileName compPath
            let (TaskLengthFile lenPath) = compToTaskLength compFile
            settings <- runExceptT $ readCompSettings compPath
            case settings of
                Left msg -> print msg
                Right settings' -> do
                    let zs = zones <$> tasks settings'
                    let includeTask = if null task then const True else flip elem task

                    let ts = taskTracks noTaskWaypoints includeTask measure zs
                    writeTaskLength ts lenPath

            where
                writeTaskLength :: [Maybe TZ.TaskTrack] -> FilePath -> IO ()
                writeTaskLength os yamlPath = do
                    let tzi =
                            TZ.TaskRoutes { taskRoutes = os }

                    let yaml =
                            Y.encodePretty
                                (Y.setConfCompare cmp Y.defConfig)
                                tzi 

                    BS.writeFile yamlPath yaml
