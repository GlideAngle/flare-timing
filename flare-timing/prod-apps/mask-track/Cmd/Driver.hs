{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Cmd.Driver (driverMain) where

import Data.String (IsString)
import Control.Monad (mapM_)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (FilePath, takeFileName, replaceExtension, dropExtension)
import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..), Reckon(..))
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS

import qualified Data.Flight.Comp as Cmp (CompSettings(..), Pilot(..))
import qualified Flight.Task as Tsk (TaskDistance(..))
import qualified Flight.Score as Gap (PilotDistance(..), PilotTime(..))
import Data.Flight.TrackLog (TrackFileFail(..), IxTask(..))
import Flight.Units ()
import Flight.Mask (Masking)
import Flight.Mask.Pilot
    ( checkTracks
    , madeZones
    , launched
    , madeGoal
    , distanceToGoal
    , distanceFlown
    , timeFlown
    )
import qualified Data.Flight.PilotTrack as TZ
    (FlownTrack(..), PilotFlownTrack(..), PilotTracks(..))
import Data.Number.RoundingFunctions (dpRound)

type MkPart a =
    FilePath
    -> [IxTask]
    -> [Cmp.Pilot]
    -> ExceptT
        String
        IO
        [[Either
            (Cmp.Pilot, TrackFileFail)
            (Cmp.Pilot, a)
        ]]

type AddPart a = a -> TZ.FlownTrack

type MkFlownTrackIO a =
    FilePath
    -> MkPart a
    -> AddPart a
    -> IO ()

driverMain :: IO ()
driverMain = withCmdArgs drive

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        ("launched", _) -> LT
        ("madeGoal", "launched") -> GT
        ("madeGoal", _) -> LT
        ("zonesMade", "launched") -> GT
        ("zonesMade", "madeGoal") -> GT
        ("zonesMade", _) -> LT
        ("zonesProof", "launched") -> GT
        ("zonesProof", "madeGoal") -> GT
        ("zonesProof", "zonesMade") -> GT
        ("zonesProof", _) -> LT
        ("timeToGoal", "launched") -> GT
        ("timeToGoal", "madeGoal") -> GT
        ("timeToGoal", "zonesMade") -> GT
        ("timeToGoal", "zonesProof") -> GT
        ("timeToGoal", _) -> LT
        ("distanceToGoal", "launched") -> GT
        ("distanceToGoal", "madeGoal") -> GT
        ("distanceToGoal", "zonesMade") -> GT
        ("distanceToGoal", "zonesProof") -> GT
        ("distanceToGoal", "timeToGoal") -> GT
        ("distanceToGoal", _) -> LT
        ("bestDistance", _) -> GT
        ("time", _) -> LT
        ("lat", "time") -> GT
        ("lat", _) -> LT
        ("lng", _) -> GT
        _ -> compare a b

unTaskDistance :: Fractional a => Tsk.TaskDistance -> a
unTaskDistance (Tsk.TaskDistance d) =
    fromRational $ dpRound 3 dKm
    where 
        MkQuantity dKm = convert d :: Quantity Rational [u| km |]

unPilotDistance :: Fractional a => Gap.PilotDistance -> a
unPilotDistance (Gap.PilotDistance d) =
    fromRational $ dpRound 3 dKm
    where 
        d' :: Quantity Rational [u| m |] = MkQuantity d
        MkQuantity dKm = convert d' :: Quantity Rational [u| km |]

unPilotTime :: Fractional a => Gap.PilotTime -> a
unPilotTime (Gap.PilotTime t) = fromRational t

drive :: CmdOptions -> IO ()
drive CmdOptions{..} = do
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
    where
        withFile yamlCompPath = do
            putStrLn $ takeFileName yamlCompPath
            let yamlMaskPath =
                    flip replaceExtension ".mask-track.yaml"
                    $ dropExtension yamlCompPath
            case reckon of
                All ->
                    let go = writeMask yamlMaskPath
                    in go checkAll id

                Zones ->
                    let go = writeMask yamlMaskPath
                    in go checkZones (\ (zs, zp) ->
                        TZ.FlownTrack
                            { launched = True
                            , madeGoal = True
                            , zonesMade = zs
                            , zonesProof = zp
                            , timeToGoal = Nothing
                            , distanceToGoal = Nothing
                            , bestDistance = Nothing
                            })

                Launch ->
                    let go = writeMask yamlMaskPath
                    in go checkLaunched (\x ->
                        TZ.FlownTrack
                            { launched = x
                            , madeGoal = True
                            , zonesMade = []
                            , zonesProof = []
                            , timeToGoal = Nothing
                            , distanceToGoal = Nothing
                            , bestDistance = Nothing
                            })

                Goal ->
                    let go = writeMask yamlMaskPath
                    in go checkMadeGoal (\x ->
                        TZ.FlownTrack
                            { launched = True
                            , madeGoal = x
                            , zonesMade = [] 
                            , zonesProof = []
                            , timeToGoal = Nothing
                            , distanceToGoal = Nothing
                            , bestDistance = Nothing
                            })

                GoalDistance ->
                    let go = writeMask yamlMaskPath
                    in go checkDistanceToGoal (\td ->
                        TZ.FlownTrack
                            { launched = True
                            , madeGoal = True
                            , zonesMade = []
                            , zonesProof = []
                            , timeToGoal = Nothing
                            , distanceToGoal = unTaskDistance <$> td
                            , bestDistance = Nothing 
                            })

                FlownDistance ->
                    let go = writeMask yamlMaskPath
                    in go checkDistanceFlown (\fd ->
                        TZ.FlownTrack
                            { launched = True
                            , madeGoal = True
                            , zonesMade = []
                            , zonesProof = []
                            , timeToGoal = Nothing
                            , distanceToGoal = Nothing
                            , bestDistance = unPilotDistance <$> fd
                            })

                Time ->
                    let go = writeMask yamlMaskPath
                    in go checkTimeToGoal (\ttg ->
                        TZ.FlownTrack
                            { launched = True
                            , madeGoal = True
                            , zonesMade = []
                            , zonesProof = []
                            , timeToGoal = unPilotTime <$> ttg
                            , distanceToGoal = Nothing
                            , bestDistance = Nothing
                            })

                Lead -> putStrLn "TODO: Handle reckoning of 'lead'."

            where
                writeMask :: forall a. MkFlownTrackIO a
                writeMask yamlPath f g = do
                    checks <-
                        runExceptT $
                            f
                                yamlCompPath
                                (IxTask <$> task)
                                (Cmp.Pilot <$> pilot)

                    case checks of
                        Left msg -> print msg
                        Right xs -> do
                            let ps :: [[TZ.PilotFlownTrack]] =
                                    (fmap . fmap)
                                        (\case
                                            Left (p, _) ->
                                                TZ.PilotFlownTrack p Nothing

                                            Right (p, x) ->
                                                TZ.PilotFlownTrack p (Just $ g x))
                                        xs

                            let tzi =
                                    TZ.PilotTracks { pilotTracks = ps }

                            let yaml =
                                    Y.encodePretty
                                        (Y.setConfCompare cmp Y.defConfig)
                                        tzi 

                            BS.writeFile yamlPath yaml

                checkLaunched =
                    checkTracks $ \Cmp.CompSettings{tasks} -> launched tasks

                checkMadeGoal =
                    checkTracks $ \Cmp.CompSettings{tasks} -> madeGoal tasks

                checkZones =
                    checkTracks $ \Cmp.CompSettings{tasks} -> madeZones tasks

                checkDistanceToGoal =
                    checkTracks $ \Cmp.CompSettings{tasks} -> distanceToGoal tasks

                checkDistanceFlown =
                    checkTracks $ \Cmp.CompSettings{tasks} -> distanceFlown tasks

                checkTimeToGoal =
                    checkTracks $ \Cmp.CompSettings{tasks} -> timeFlown tasks

                checkAll =
                    checkTracks $ \Cmp.CompSettings{tasks} -> flown tasks

                flown :: Masking TZ.FlownTrack
                flown tasks iTask xs =
                    let ld = launched tasks iTask xs
                        mg = madeGoal tasks iTask xs
                        (zs, zp) = madeZones tasks iTask xs
                        dg = distanceToGoal tasks iTask xs
                        df = distanceFlown tasks iTask xs
                        tf = timeFlown tasks iTask xs
                    in TZ.FlownTrack
                        { launched = ld
                        , madeGoal = mg
                        , zonesMade = zs
                        , zonesProof = zp

                        , distanceToGoal =
                            if mg then Nothing else unTaskDistance <$> dg

                        , bestDistance =
                            if mg then Nothing else unPilotDistance <$> df

                        , timeToGoal =
                            if not mg then Nothing else unPilotTime <$> tf
                        }
