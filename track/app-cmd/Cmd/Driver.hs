{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Cmd.Driver (driverMain) where

import Data.List (findIndex)
import Data.Ratio ((%))
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Control.Lens ((^?), element)
import Control.Monad (mapM_)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (FilePath, takeFileName, takeDirectory)

import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..), Reckon(..))
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)

import qualified Data.Flight.Kml as Kml (Fix, LatLngAlt(..))
import qualified Data.Flight.Comp as Cmp
    ( CompSettings(..)
    , Pilot(..)
    , Task(..)
    , Zone(..)
    , PilotTrackLogFile(..)
    , Latitude(..)
    , Longitude(..)
    )
import Data.Flight.TrackLog as Log
    ( TrackFileFail(..)
    , IxTask(..)
    , pilotTracks
    , filterPilots
    , filterTasks
    , makeAbsolute
    )
import Flight.Task as Tsk
    ( Lat(..)
    , Lng(..)
    , LatLng(..)
    , Radius(..)
    , Zone(..)
    , separatedZones
    )
import Flight.Units ()

newtype PilotTrackFixes = PilotTrackFixes Int deriving Show

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
                Fixes -> do
                    made <- runExceptT $ checkFixes
                                            yamlCompPath
                                            (IxTask <$> task)
                                            (Cmp.Pilot <$> pilot)
                    case made of
                        Left msg -> print msg
                        Right fixCounts -> print fixCounts

                Zones -> do
                    made <- runExceptT $ checkZones
                                            yamlCompPath
                                            (IxTask <$> task)
                                            (Cmp.Pilot <$> pilot)
                    case made of
                        Left msg -> print msg
                        Right zoneHits -> print zoneHits

                Launch -> do
                    made <- runExceptT $ checkLaunched
                                            yamlCompPath
                                            (IxTask <$> task)
                                            (Cmp.Pilot <$> pilot)
                    case made of
                        Left msg -> print msg
                        Right departures -> print departures

                Goal -> do
                    made <- runExceptT $ checkMadeGoal
                                            yamlCompPath
                                            (IxTask <$> task)
                                            (Cmp.Pilot <$> pilot)
                    case made of
                        Left msg -> print msg
                        Right arrivals -> print arrivals

                x ->
                    putStrLn $ "TODO: Handle other reckon of " ++ show x

readSettings :: FilePath -> ExceptT String IO Cmp.CompSettings
readSettings compYamlPath = do
    contents <- lift $ BS.readFile compYamlPath
    ExceptT . return $ decodeEither contents

settingsLogs :: FilePath
             -> [IxTask]
             -> [Cmp.Pilot]
             -> ExceptT String IO (Cmp.CompSettings, [[Cmp.PilotTrackLogFile]])
settingsLogs compYamlPath tasks selectPilots = do
    settings <- readSettings compYamlPath
    ExceptT . return $ go settings
    where
        go s@(Cmp.CompSettings {pilots, taskFolders}) =
            Right (s, zs)
            where
                dir = takeDirectory compYamlPath
                ys = Log.filterPilots selectPilots $ Log.filterTasks tasks pilots
                fs = (Log.makeAbsolute dir) <$> taskFolders
                zs = zipWith (\f y -> f <$> y) fs ys

checkTracks :: forall a. (Cmp.CompSettings -> (IxTask -> [Kml.Fix] -> a))
            -> FilePath
            -> [IxTask]
            -> [Cmp.Pilot]
            -> ExceptT
                String
                IO
                [[ Either
                   (Cmp.Pilot, TrackFileFail)
                   (Cmp.Pilot, a)
                ]]
checkTracks f compYamlPath tasks selectPilots = do
    (settings, xs) <- settingsLogs compYamlPath tasks selectPilots
    lift $ Log.pilotTracks (f settings) xs

checkFixes :: FilePath
           -> [IxTask]
           -> [Cmp.Pilot]
           -> ExceptT
               String
               IO
               [[ Either
                   (Cmp.Pilot, TrackFileFail)
                   (Cmp.Pilot, PilotTrackFixes)
               ]]
checkFixes =
    checkTracks $ const (\_ xs -> countFixes xs)

checkZones :: FilePath
           -> [IxTask]
           -> [Cmp.Pilot]
           -> ExceptT
               String
               IO
               [[ Either
               (Cmp.Pilot, TrackFileFail)
               (Cmp.Pilot, [Bool])
               ]]
checkZones =
    checkTracks $ \(Cmp.CompSettings {tasks}) -> madeZones tasks

checkLaunched :: FilePath
              -> [IxTask]
              -> [Cmp.Pilot]
              -> ExceptT
                  String
                  IO
                  [[ Either
                      (Cmp.Pilot, TrackFileFail)
                      (Cmp.Pilot, Bool)
                  ]]
checkLaunched =
    checkTracks $ \(Cmp.CompSettings {tasks}) -> launched tasks

checkMadeGoal :: FilePath
              -> [IxTask]
              -> [Cmp.Pilot]
              -> ExceptT
                  String
                  IO
                  [[ Either
                      (Cmp.Pilot, TrackFileFail)
                      (Cmp.Pilot, Bool)
                  ]]
checkMadeGoal =
    checkTracks $ \(Cmp.CompSettings {tasks}) -> madeGoal tasks

countFixes :: [Kml.Fix] -> PilotTrackFixes
countFixes xs = PilotTrackFixes $ length xs

-- | The input pair is in degrees while the output is in radians.
toLL :: (Rational, Rational) -> Tsk.LatLng [u| rad |]
toLL (lat, lng) =
    Tsk.LatLng (Tsk.Lat lat'', Tsk.Lng lng'')
        where
            lat' = (MkQuantity lat) :: Quantity Rational [u| deg |]
            lng' = (MkQuantity lng) :: Quantity Rational [u| deg |]
            lat'' = convert lat' :: Quantity Rational [u| rad |]
            lng'' = convert lng' :: Quantity Rational [u| rad |]

zoneToCylinder :: Cmp.Zone -> Tsk.Zone
zoneToCylinder z =
    Tsk.Cylinder radius (toLL(lat, lng))
    where
        radius = Radius (MkQuantity $ Cmp.radius z % 1)
        Cmp.Latitude lat = Cmp.lat z
        Cmp.Longitude lng = Cmp.lng z

fixToPoint :: Kml.Fix -> Tsk.Zone
fixToPoint fix =
    Tsk.Point (toLL (lat, lng))
    where
        lat = Kml.lat fix
        lng = Kml.lng fix

crossedZone :: Tsk.Zone -> [Kml.Fix] -> Bool
crossedZone z xs =
    entersZone z xs || exitsZone z xs

entersZone :: Tsk.Zone -> [Kml.Fix] -> Bool
entersZone z xs =
    exitsZone z $ reverse xs

exitsZone :: Tsk.Zone -> [Kml.Fix] -> Bool
exitsZone z xs =
    case (insideZone, outsideZone) of
        (Just _, Just _) -> True
        _ -> False
    where
        ys :: [Tsk.Zone]
        ys = fixToPoint <$> xs

        insideZone :: Maybe Int
        insideZone =
            findIndex (\y -> not $ Tsk.separatedZones [y, z]) ys

        outsideZone :: Maybe Int
        outsideZone =
            findIndex (\y -> Tsk.separatedZones [y, z]) ys

launched :: [Cmp.Task] -> IxTask -> [Kml.Fix] -> Bool
launched tasks (IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing -> False
        Just (Cmp.Task {zones})->
            case zones of
                [] -> False
                z : _ -> exitsZone (zoneToCylinder z) xs

madeGoal :: [Cmp.Task] -> IxTask -> [Kml.Fix] -> Bool
madeGoal tasks (IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing -> False
        Just (Cmp.Task {zones})->
            case reverse $ zones of
                [] -> False
                z : _ -> entersZone (zoneToCylinder z) xs

madeZones :: [Cmp.Task] -> IxTask -> [Kml.Fix] -> [Bool]
madeZones tasks (IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing -> []
        Just (Cmp.Task {zones}) ->
            (flip crossedZone xs . zoneToCylinder) <$> zones
