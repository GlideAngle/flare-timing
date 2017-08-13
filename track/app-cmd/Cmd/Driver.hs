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
{-# LANGUAGE LambdaCase #-}

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
    , SpeedSection
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
    , TaskDistance(..)
    , EdgeDistance(..)
    , Tolerance(..)
    , DistancePath(..)
    , separatedZones
    , distanceEdgeToEdge
    )
import Flight.Units ()

newtype PilotTrackFixes = PilotTrackFixes Int deriving Show

driverMain :: IO ()
driverMain = withCmdArgs drive

drive :: CmdOptions -> IO ()
drive CmdOptions{..} = do
    dfe <- doesFileExist file
    if dfe then
        withFile file
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- find always (fileType ==? RegularFile &&? extension ==? ".comp.yaml") dir
            mapM_ withFile files
        else
            putStrLn "Couldn't find any flight score competition yaml input files."
    where
        withFile yamlCompPath = do
            putStrLn $ takeFileName yamlCompPath

            case reckon of
                Fixes -> go checkFixes
                Zones -> go checkZones
                SpeedZones -> go checkSpeedZones
                Launch -> go checkLaunched
                Started -> go checkStarted
                Goal -> go checkMadeGoal
                GoalDistance -> go checkDistanceToGoal
                x -> putStrLn $ "TODO: Handle other reckon of " ++ show x

            where
                go :: Show a => (FilePath
                   -> [IxTask]
                   -> [Cmp.Pilot]
                   -> ExceptT
                       String
                       IO
                       [[Either
                           (Cmp.Pilot, TrackFileFail)
                           (Cmp.Pilot, a)
                       ]])
                   -> IO ()
                go f = do
                    checks <-
                        runExceptT $
                            f
                                yamlCompPath
                                (IxTask <$> task)
                                (Cmp.Pilot <$> pilot)

                    print checks

                checkFixes =
                    checkTracks $ const (\_ xs -> countFixes xs)

                checkZones =
                    checkTracks $ \(Cmp.CompSettings {tasks}) -> madeZones tasks

                checkSpeedZones =
                    checkTracks $ \(Cmp.CompSettings {tasks}) -> madeSpeedZones tasks

                checkLaunched =
                    checkTracks $ \(Cmp.CompSettings {tasks}) -> launched tasks

                checkStarted =
                    checkTracks $ \(Cmp.CompSettings {tasks}) -> started tasks

                checkMadeGoal =
                    checkTracks $ \(Cmp.CompSettings {tasks}) -> madeGoal tasks

                checkDistanceToGoal =
                    checkTracks $ \(Cmp.CompSettings {tasks}) -> distanceToGoal tasks

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

started :: [Cmp.Task] -> IxTask -> [Kml.Fix] -> Bool
started tasks (IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing -> False
        Just (Cmp.Task {speedSection, zones}) ->
            case slice speedSection zones of
                [] -> False
                z : _ -> exitsZone (zoneToCylinder z) xs

madeGoal :: [Cmp.Task] -> IxTask -> [Kml.Fix] -> Bool
madeGoal tasks (IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing -> False
        Just (Cmp.Task {zones}) ->
            case reverse $ zones of
                [] -> False
                z : _ -> entersZone (zoneToCylinder z) xs

tickedZones :: [Tsk.Zone] -> [Kml.Fix] -> [Bool]
tickedZones zones xs =
    flip crossedZone xs <$> zones

madeZones :: [Cmp.Task] -> IxTask -> [Kml.Fix] -> [Bool]
madeZones tasks (IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing -> []
        Just (Cmp.Task {zones}) ->
            tickedZones (zoneToCylinder <$> zones) xs

madeSpeedZones :: [Cmp.Task] -> IxTask -> [Kml.Fix] -> [Bool]
madeSpeedZones tasks (IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing -> []
        Just (Cmp.Task {speedSection, zones}) ->
            tickedZones (zoneToCylinder <$> slice speedSection zones) xs

mm30 :: Tolerance
mm30 = Tolerance $ 30 % 1000

distanceViaZones :: Cmp.SpeedSection
                 -> [Tsk.Zone]
                 -> [Kml.Fix]
                 -> Maybe TaskDistance
distanceViaZones speedSection zs xs =
    case reverse xs of
        [] -> Nothing
        -- TODO: Check all fixes from last turnpoint made.
        x : _ ->
            Just . edges $
                distanceEdgeToEdge
                    PathPointToZone
                    mm30
                    ((fixToPoint x) : notTicked)
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        ys = tickedZones zsSpeed xs
        notTicked = drop (length $ takeWhile (== True) ys) zsSpeed

slice :: Cmp.SpeedSection -> [a] -> [a]
slice = \case
    Nothing -> id
    Just (s', e') ->
        let (s, e) = (fromInteger s' - 1, fromInteger e' - 1)
        in take (e - s + 1) . drop s

distanceToGoal :: [Cmp.Task] -> IxTask -> [Kml.Fix] -> Maybe TaskDistance
distanceToGoal tasks (IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing -> Nothing
        Just (Cmp.Task {speedSection, zones}) ->
            if null zones then Nothing else
            distanceViaZones speedSection (zoneToCylinder <$> zones) xs
