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

import Prelude hiding (span)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.String (IsString)
import Control.Monad (mapM_)
import Control.Monad.Except (ExceptT(..), runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (FilePath, takeFileName, replaceExtension, dropExtension)
import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..))
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS

import qualified Flight.Comp as Cmp (CompSettings(..), Pilot(..))
import Flight.TrackLog (TrackFileFail(..), IxTask(..))
import Flight.Units ()
import Flight.Track.Cross (TrackCross(..), PilotTrackCross(..), Crossing(..))
import Flight.Zone.Raw (RawZone)
import Flight.Task (SpanLatLng)
import Flight.PointToPoint.Rational (distanceHaversine)
import Flight.LatLng.Rational (defEps)
import Flight.Mask
    ( TaskZone, SigMasking
    , unSelectedCrossings, unNomineeCrossings
    , checkTracks, madeZones, zoneToCylinder
    )

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

type AddPart a = a -> TrackCross

type MkCrossingTrackIO a =
    FilePath
    -> MkPart a
    -> AddPart a
    -> IO ()

driverMain :: IO ()
driverMain = withCmdArgs drive

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
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
                                (Cmp.Pilot <$> pilot)

                    case checks of
                        Left msg -> print msg
                        Right xs -> do
                            let ps :: [[PilotTrackCross]] =
                                    (fmap . fmap)
                                        (\case
                                            Left (p, _) ->
                                                PilotTrackCross p Nothing

                                            Right (p, x) ->
                                                PilotTrackCross p (Just $ g x))
                                        xs

                            let tzi =
                                    Crossing { crossing = ps }

                            let yaml =
                                    Y.encodePretty
                                        (Y.setConfCompare cmp Y.defConfig)
                                        tzi 

                            BS.writeFile yamlCrossPath yaml

                checkAll =
                    checkTracks $ \Cmp.CompSettings{tasks} -> flown tasks

                flown :: SigMasking TrackCross
                flown tasks iTask xs =
                    TrackCross
                        { zonesCrossSelected = unSelectedCrossings selected
                        , zonesCrossNominees = unNomineeCrossings nominees
                        }
                    where
                        (selected, nominees) =
                            madeZones span zoneToCyl tasks iTask xs

zoneToCyl :: RawZone -> TaskZone Rational
zoneToCyl = zoneToCylinder

span :: SpanLatLng Rational
span = distanceHaversine defEps
