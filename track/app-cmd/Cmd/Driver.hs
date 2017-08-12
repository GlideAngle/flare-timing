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

import qualified Data.Flight.Kml as K (Fix, LatLngAlt(..))
import qualified Data.Flight.Comp as C
    ( CompSettings(..)
    , Pilot(..)
    , Task(..)
    , Zone(..)
    , PilotTrackLogFile(..)
    , Latitude(..)
    , Longitude(..)
    )
import Data.Flight.TrackLog as T
    ( TrackFileFail(..)
    , Task
    , goalPilotTracks
    , filterPilots
    , filterTasks
    , makeAbsolute
    )
import Flight.Task as TK
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
                                            task
                                            (C.Pilot <$> pilot)
                    case made of
                        Left msg -> print msg
                        Right tracks -> print tracks

                Launch -> do
                    made <- runExceptT $ checkLaunched
                                            yamlCompPath
                                            task
                                            (C.Pilot <$> pilot)
                    case made of
                        Left msg -> print msg
                        Right tracks -> print tracks

                x ->
                    putStrLn $ "TODO: Handle other reckon of " ++ show x

readSettings :: FilePath -> ExceptT String IO C.CompSettings
readSettings compYamlPath = do
    contents <- lift $ BS.readFile compYamlPath
    ExceptT . return $ decodeEither contents

settingsLogs :: FilePath
             -> [T.Task]
             -> [C.Pilot]
             -> ExceptT String IO (C.CompSettings, [[C.PilotTrackLogFile]])
settingsLogs compYamlPath tasks selectPilots = do
    settings <- readSettings compYamlPath
    ExceptT . return $ go settings
    where
        go s@(C.CompSettings {pilots, taskFolders}) =
            Right (s, zs)
            where
                dir = takeDirectory compYamlPath
                ys = T.filterPilots selectPilots $ T.filterTasks tasks pilots
                fs = (T.makeAbsolute dir) <$> taskFolders
                zs = zipWith (\f y -> f <$> y) fs ys

checkFixes :: FilePath
           -> [T.Task]
           -> [C.Pilot]
           -> ExceptT
               String
               IO
               [[ Either
                   (C.Pilot, TrackFileFail)
                   (C.Pilot, PilotTrackFixes)
               ]]
checkFixes compYamlPath tasks selectPilots = do
    (_, zs) <- settingsLogs compYamlPath tasks selectPilots
    lift $ T.goalPilotTracks (\_ xs -> countFixes xs) zs

checkLaunched :: FilePath
              -> [T.Task]
              -> [C.Pilot]
              -> ExceptT
                  String
                  IO
                  [[ Either
                      (C.Pilot, TrackFileFail)
                      (C.Pilot, Bool)
                  ]]
checkLaunched compYamlPath ts selectPilots = do
    (C.CompSettings {tasks}, zs) <- settingsLogs compYamlPath ts selectPilots
    lift $ T.goalPilotTracks (launched tasks) zs

countFixes :: [K.Fix] -> PilotTrackFixes
countFixes xs = PilotTrackFixes $ length xs

-- | The input pair is in degrees while the output is in radians.
toLL :: (Rational, Rational) -> TK.LatLng [u| rad |]
toLL (lat, lng) =
    TK.LatLng (TK.Lat lat'', TK.Lng lng'')
        where
            lat' = (MkQuantity lat) :: Quantity Rational [u| deg |]
            lng' = (MkQuantity lng) :: Quantity Rational [u| deg |]
            lat'' = convert lat' :: Quantity Rational [u| rad |]
            lng'' = convert lng' :: Quantity Rational [u| rad |]

zoneToCylinder :: C.Zone -> TK.Zone
zoneToCylinder z =
    TK.Cylinder radius (toLL(lat, lng))
    where
        radius = Radius (MkQuantity $ C.radius z % 1)
        C.Latitude lat = C.lat z
        C.Longitude lng = C.lng z

fixToPoint :: K.Fix -> TK.Zone
fixToPoint fix =
    TK.Point (toLL (lat, lng))
    where
        lat = K.lat fix
        lng = K.lng fix

exitsZone :: TK.Zone -> [K.Fix] -> Bool
exitsZone startCyl xs =
    case (insideZone, outsideZone) of
        (Just i, Just j) -> True
        _ -> False
    where
        ys :: [TK.Zone]
        ys = fixToPoint <$> xs

        insideZone :: Maybe Int
        insideZone =
            findIndex (\y -> not $ TK.separatedZones [y, startCyl]) ys

        outsideZone :: Maybe Int
        outsideZone =
            findIndex (\y -> TK.separatedZones [y, startCyl]) ys

launched :: [C.Task] -> Int -> [K.Fix] -> Bool
launched tasks i xs =
    case tasks ^? element i of
        Nothing -> False
        Just (C.Task {zones})->
            case zones of
                [] -> False
                z : _ -> exitsZone (zoneToCylinder z) xs
