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

import qualified Data.List as List (find, findIndex)
import Data.Ratio ((%))
import Data.UnitsOfMeasure ((-:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..), unQuantity)
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

import qualified Data.Flight.Kml as Kml (Fix, LatLngAlt(..), FixMark(mark))
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
import Flight.Score as Gap
    ( PilotDistance(..)
    , PilotTime(..)
    , Seconds
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
                FlownDistance -> go checkDistanceFlown
                Time -> go checkTimeToGoal
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

                checkDistanceFlown =
                    checkTracks $ \(Cmp.CompSettings {tasks}) -> distanceFlown tasks

                checkTimeToGoal =
                    checkTracks $ \(Cmp.CompSettings {tasks}) -> timeFlown tasks

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

crossedZone :: Tsk.Zone -> [Tsk.Zone] -> Bool
crossedZone z xs =
    entersZone z xs || exitsZone z xs

entersZone :: Tsk.Zone -> [Tsk.Zone] -> Bool
entersZone z xs =
    exitsZone z $ reverse xs

exitsZone :: Tsk.Zone -> [Tsk.Zone] -> Bool
exitsZone z xs =
    case (insideZone, outsideZone) of
        (Just _, Just _) -> True
        _ -> False
    where
        insideZone :: Maybe Int
        insideZone =
            List.findIndex (\y -> not $ Tsk.separatedZones [y, z]) xs

        outsideZone :: Maybe Int
        outsideZone =
            List.findIndex (\y -> Tsk.separatedZones [y, z]) xs

launched :: [Cmp.Task] -> IxTask -> [Kml.Fix] -> Bool
launched tasks (IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing -> False
        Just (Cmp.Task {zones})->
            case zones of
                [] -> False
                z : _ -> exitsZone (zoneToCylinder z) (fixToPoint <$> xs)

started :: [Cmp.Task] -> IxTask -> [Kml.Fix] -> Bool
started tasks (IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing -> False
        Just (Cmp.Task {speedSection, zones}) ->
            case slice speedSection zones of
                [] -> False
                z : _ -> exitsZone (zoneToCylinder z) (fixToPoint <$> xs)

madeGoal :: [Cmp.Task] -> IxTask -> [Kml.Fix] -> Bool
madeGoal tasks (IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing -> False
        Just (Cmp.Task {zones}) ->
            case reverse $ zones of
                [] -> False
                z : _ -> entersZone (zoneToCylinder z) (fixToPoint <$> xs)

tickedZones :: [Tsk.Zone] -> [Tsk.Zone] -> [Bool]
tickedZones zones xs =
    flip crossedZone xs <$> zones

madeZones :: [Cmp.Task] -> IxTask -> [Kml.Fix] -> [Bool]
madeZones tasks (IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing -> []
        Just (Cmp.Task {zones}) ->
            tickedZones (zoneToCylinder <$> zones) (fixToPoint <$> xs)

madeSpeedZones :: [Cmp.Task] -> IxTask -> [Kml.Fix] -> [Bool]
madeSpeedZones tasks (IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing -> []
        Just (Cmp.Task {speedSection, zones}) ->
            tickedZones
                (zoneToCylinder <$> slice speedSection zones)
                (fixToPoint <$> xs)

mm30 :: Tolerance
mm30 = Tolerance $ 30 % 1000

distanceViaZones :: (a -> Zone)
                 -> Cmp.SpeedSection
                 -> [Tsk.Zone]
                 -> [a]
                 -> Maybe TaskDistance
distanceViaZones mkZone speedSection zs xs =
    case reverse xs of
        [] -> Nothing
        -- TODO: Check all fixes from last turnpoint made.
        x : _ ->
            Just . edges $
                distanceEdgeToEdge
                    PathPointToZone
                    mm30
                    ((mkZone x) : notTicked)
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        ys = tickedZones zsSpeed (mkZone <$> xs)
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
            distanceViaZones
                fixToPoint
                speedSection
                (zoneToCylinder <$> zones)
                xs

distanceFlown :: [Cmp.Task] -> IxTask -> [Kml.Fix] -> Maybe PilotDistance
distanceFlown tasks (IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing -> Nothing
        Just (Cmp.Task {speedSection, zones}) ->
            if null zones then Nothing else
            let cs = zoneToCylinder <$> zones
                d = distanceViaZones fixToPoint speedSection cs xs
            in flownDistance speedSection cs d

flownDistance :: Cmp.SpeedSection
              -> [Tsk.Zone]
              -> Maybe TaskDistance
              -> Maybe PilotDistance
flownDistance _ [] (Just (TaskDistance (MkQuantity d))) =
    Just $ PilotDistance d
flownDistance _ _ Nothing = Nothing
flownDistance speedSection zs@(z : _) (Just (TaskDistance d)) =
    case total of
        Nothing ->
            Nothing

        Just (TaskDistance dMax) ->
            Just . PilotDistance . unQuantity $ dMax -: d
    where
        total = distanceViaZones id speedSection zs [z]

timeFlown :: [Cmp.Task] -> IxTask -> [Kml.Fix] -> Maybe PilotTime
timeFlown tasks iTask@(IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing -> Nothing
        Just (Cmp.Task {speedSection, zones}) ->
            if null zones then Nothing else
            if not atGoal then Nothing else
            let cs = zoneToCylinder <$> zones
            in flownDuration speedSection cs xs
    where
        atGoal = madeGoal tasks iTask xs

flownDuration :: Cmp.SpeedSection
              -> [Tsk.Zone]
              -> [Kml.Fix]
              -> Maybe PilotTime
flownDuration _ [] _ = Nothing
flownDuration _ _ [] = Nothing
flownDuration speedSection zs xs =
    durationViaZones fixToPoint Kml.mark speedSection zs xs

durationViaZones :: (Kml.Fix -> Tsk.Zone)
                 -> (Kml.Fix -> Seconds)
                 -> Cmp.SpeedSection
                 -> [Tsk.Zone]
                 -> [Kml.Fix]
                 -> Maybe PilotTime
durationViaZones mkZone atTime speedSection zs xs =
    case (zsSpeed, reverse zsSpeed) of
        ([], _) -> Nothing
        (_, []) -> Nothing
        (z0 : _, zN : _) -> duration (z0, zN) xys
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs

        xys :: [(Kml.Fix, (Tsk.Zone, Tsk.Zone))]
        xys = (\(x, y) -> (y, (mkZone x, mkZone y))) <$> zip (drop 1 xs) xs

        slots :: (Tsk.Zone, Tsk.Zone)
              -> [(Kml.Fix, (Tsk.Zone, Tsk.Zone))]
              -> (Maybe Seconds, Maybe Seconds)
        slots (z0, zN) xzs =
            (f <$> xz0, f <$> xzN)
            where
                exits' :: (Kml.Fix, (Tsk.Zone, Tsk.Zone)) -> Bool
                exits' (_, (zx, zy)) = exitsZone z0 [zx, zy]

                enters' :: (Kml.Fix, (Tsk.Zone, Tsk.Zone)) -> Bool
                enters' (_, (zx, zy)) = entersZone zN [zx, zy]

                xz0 :: Maybe (Kml.Fix, (Tsk.Zone, Tsk.Zone))
                xz0 = List.find exits' xzs

                xzN :: Maybe (Kml.Fix, (Tsk.Zone, Tsk.Zone))
                xzN = List.find enters' xzs

                f = atTime . fst

        duration z xzs =
            case slots z xzs of
                (Nothing, _) -> Nothing
                (_, Nothing) -> Nothing
                (Just t0, Just tN) ->
                    Just . PilotTime . toRational $ tN - t0
