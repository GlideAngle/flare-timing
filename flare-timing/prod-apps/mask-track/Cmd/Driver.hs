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
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cmd.Driver (driverMain) where

import Prelude hiding (span)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.String (IsString)
import Control.Monad (mapM_)
import Control.Monad.Except (runExceptT)
import Data.UnitsOfMeasure ((/:), u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (takeFileName, replaceExtension, dropExtension)
import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..))
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS
import qualified Data.Number.FixedFunctions as F

import qualified Flight.Comp as Cmp (CompSettings(..), Pilot(..))
import qualified Flight.Task as Tsk (TaskDistance(..))
import qualified Flight.Score as Gap (PilotDistance(..), PilotTime(..))
import Flight.TrackLog (IxTask(..))
import Flight.Units ()
import Flight.Mask
    ( SigMasking
    , TaskZone
    , checkTracks
    , madeGoal
    , distanceToGoal
    , distanceFlown
    , timeFlown
    , zoneToCylinder
    )
import Flight.Track.Mask (Masking(..), PilotTrackMask(..))
import qualified Flight.Track.Mask as TM (TrackMask(..))
import Flight.Zone (Bearing(..))
import Flight.Zone.Raw (RawZone)
import Flight.LatLng.Rational (Epsilon(..), defEps)
import Data.Number.RoundingFunctions (dpRound)
import Flight.Task (SpanLatLng, CircumSample, AngleCut(..))
import Flight.PointToPoint.Rational
    (distanceHaversine, distancePointToPoint, costSegment)
import Flight.Cylinder.Rational (circumSample)

driverMain :: IO ()
driverMain = withCmdArgs drive

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        ("timing", _) -> LT
        ("masking", _) -> GT
        ("firstStart", _) -> LT
        ("lastGoal", _) -> GT
        ("launched", _) -> LT
        ("madeGoal", "launched") -> GT
        ("madeGoal", _) -> LT
        ("timeToGoal", "launched") -> GT
        ("timeToGoal", "madeGoal") -> GT
        ("timeToGoal", _) -> LT
        ("distanceToGoal", "launched") -> GT
        ("distanceToGoal", "madeGoal") -> GT
        ("distanceToGoal", "timeToGoal") -> GT
        ("distanceToGoal", _) -> LT
        ("distanceMade", _) -> GT
        ("zonesTime", _) -> GT
        ("time", _) -> LT
        ("lat", "time") -> GT
        ("lat", _) -> LT
        ("lng", _) -> GT
        _ -> compare a b

unTaskDistance :: (Real a, Fractional a) => Tsk.TaskDistance a -> a
unTaskDistance (Tsk.TaskDistance d) =
    fromRational $ dpRound 3 dKm
    where 
        MkQuantity dKm = toRational' $ convert d :: Quantity _ [u| km |]

unPilotDistance :: (Real a, Fractional b) => Gap.PilotDistance a -> b
unPilotDistance (Gap.PilotDistance d) =
    fromRational $ dpRound 3 dKm
    where 
        d' :: Quantity Rational [u| m |] = MkQuantity $ toRational d
        MkQuantity dKm = convert d' :: Quantity Rational [u| km |]

unPilotTime :: Fractional a => Gap.PilotTime -> a
unPilotTime (Gap.PilotTime t) = fromRational t

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
    fprint ("Masking tracks completed in " % timeSpecs % "\n") start end
    where
        withFile yamlCompPath = do
            let yamlMaskPath =
                    flip replaceExtension ".mask-track.yaml"
                    $ dropExtension yamlCompPath

            putStrLn $ "Reading competition from '" ++ takeFileName yamlCompPath ++ "'"
            writeMask yamlMaskPath check

            where
                writeMask yamlMaskPath f = do
                    comp <-
                        runExceptT $
                            f
                                yamlCompPath
                                (IxTask <$> task)
                                (Cmp.Pilot <$> pilot)

                    case comp of
                        Left msg -> print msg
                        Right comp' -> do
                            let pss :: [[PilotTrackMask]] =
                                    (fmap . fmap)
                                        (\case
                                            Left (p, _) ->
                                                PilotTrackMask p Nothing

                                            Right (p, x) ->
                                                PilotTrackMask p (Just x))
                                        comp'

                            let tzi = Masking { masking = pss }

                            let yaml =
                                    Y.encodePretty
                                        (Y.setConfCompare cmp Y.defConfig)
                                        tzi 

                            BS.writeFile yamlMaskPath yaml

                check =
                    checkTracks $ \Cmp.CompSettings{tasks} -> flown tasks

flown :: SigMasking TM.TrackMask
flown tasks iTask xs =
    let mg =
            madeGoal
                span
                zoneToCyl tasks iTask xs

        dg =
            distanceToGoal
                span dpp cseg cs cut
                zoneToCyl tasks iTask xs

        df =
            distanceFlown
                span dpp cseg cs cut
                zoneToCyl tasks iTask xs

        tf =
            timeFlown
                span
                zoneToCyl tasks iTask xs

        dpp =
            distancePointToPoint

        cseg =
            costSegment span

    in TM.TrackMask
        { madeGoal = mg

        , distanceToGoal =
            if mg then Nothing else fromRational . unTaskDistance <$> dg

        , distanceMade =
            fromRational <$> if mg then Nothing else unPilotDistance <$> df

        , timeToGoal =
            if not mg then Nothing else unPilotTime <$> tf
        }

zoneToCyl :: RawZone -> TaskZone Rational
zoneToCyl = zoneToCylinder

span :: SpanLatLng Rational
span = distanceHaversine defEps

cs :: CircumSample Rational
cs = circumSample

cut :: AngleCut Rational
cut =
    AngleCut
        { sweep = let (Epsilon e) = defEps in Bearing . MkQuantity $ F.pi e
        , nextSweep = nextCut
        }

nextCut :: AngleCut Rational -> AngleCut Rational
nextCut x@AngleCut{sweep} =
    let (Bearing b) = sweep in x{sweep = Bearing $ b /: 2}
