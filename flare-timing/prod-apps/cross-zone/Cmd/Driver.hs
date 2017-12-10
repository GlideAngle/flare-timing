{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Cmd.Driver (driverMain) where

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.String (IsString)
import Data.Maybe (catMaybes)
import Control.Monad (mapM_)
import Control.Monad.Except (ExceptT(..), runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (FilePath, takeFileName, replaceExtension, dropExtension)
import Flight.Cmd.Paths (checkPaths)
import Flight.Cmd.Options (Math(..), CmdOptions(..), ProgramName(..), mkOptions)
import Cmd.Options (description)
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS

import Flight.Comp (CompSettings(..), Pilot(..), TrackFileFail(..))
import Flight.TrackLog (IxTask(..))
import Flight.Units ()
import Flight.Track.Cross (TrackCross(..), PilotTrackCross(..), Crossing(..))
import Flight.Zone.Raw (RawZone)
import qualified Flight.PointToPoint.Rational as Rat (distanceHaversine)
import qualified Flight.PointToPoint.Double as Dbl (distanceHaversine)
import Flight.LatLng.Rational (defEps)
import Flight.Mask
    ( TaskZone, SigMasking
    , unSelectedCrossings, unNomineeCrossings
    , checkTracks, madeZones, zoneToCylinder
    )

type MkPart a =
    FilePath
    -> [IxTask]
    -> [Pilot]
    -> ExceptT
        String
        IO
        [[Either
            (Pilot, TrackFileFail)
            (Pilot, a)
        ]]

type AddPart a = a -> TrackCross

type MkCrossingTrackIO a =
    FilePath
    -> MkPart a
    -> AddPart a
    -> IO ()

driverMain :: IO ()
driverMain = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing
    err <- checkPaths options
    case err of
        Just msg -> putStrLn msg
        Nothing -> drive options

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        ("errors", _) -> LT
        ("crossings", _) -> GT
        ("zonesCrossSelected", _) -> LT
        ("zonesCrossNominees", _) -> GT
        ("time", _) -> LT
        ("lat", "time") -> GT
        ("lat", _) -> LT
        ("lng", _) -> GT
        _ -> compare a b

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
    fprint ("Tracks crossing zones completed in " % timeSpecs % "\n") start end
    where
        withFile yamlCompPath = do
            let yamlCrossPath =
                    flip replaceExtension ".cross-zone.yaml"
                    $ dropExtension yamlCompPath

            putStrLn $ "Reading competition from '" ++ takeFileName yamlCompPath ++ "'"
            let go = writeMask yamlCrossPath in go checkAll id
            where
                writeMask :: forall a. MkCrossingTrackIO a
                writeMask yamlCrossPath f g = do
                    checks <-
                        runExceptT $
                            f
                                yamlCompPath
                                (IxTask <$> task)
                                (Pilot <$> pilot)

                    case checks of
                        Left msg -> print msg
                        Right xs -> do
                            let ps :: [([PilotTrackCross], [Maybe (Pilot, TrackFileFail)])] =
                                    unzip <$>
                                    (fmap . fmap)
                                        (\case
                                            Left err@(p, _) ->
                                                (PilotTrackCross p Nothing, Just err)

                                            Right (p, x) ->
                                                (PilotTrackCross p (Just $ g x), Nothing))
                                        xs

                            let tzi =
                                    Crossing { crossing = fst <$> ps
                                             , errors = catMaybes . snd <$> ps
                                             }

                            let yaml =
                                    Y.encodePretty
                                        (Y.setConfCompare cmp Y.defConfig)
                                        tzi 

                            BS.writeFile yamlCrossPath yaml

                checkAll =
                    checkTracks $ \CompSettings{tasks} -> flown math tasks

flown :: Math -> SigMasking TrackCross
flown math tasks iTask xs =
    TrackCross
        { zonesCrossSelected = unSelectedCrossings selected
        , zonesCrossNominees = unNomineeCrossings nominees
        }
    where
        (selected, nominees) =
            f math tasks iTask xs

        f = \case
            Rational ->
                madeZones
                    (Rat.distanceHaversine defEps)
                    (zoneToCylinder :: RawZone -> TaskZone Rational)
            Floating ->
                madeZones
                    Dbl.distanceHaversine
                    (zoneToCylinder :: RawZone -> TaskZone Double)
