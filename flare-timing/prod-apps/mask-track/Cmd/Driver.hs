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

import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.String (IsString)
import Control.Monad (mapM_)
import Control.Monad.Except (runExceptT)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (takeFileName, replaceExtension, dropExtension)
import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..))
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS

import qualified Data.Flight.Comp as Cmp (CompSettings(..), Pilot(..))
import qualified Flight.Task as Tsk (TaskDistance(..))
import qualified Flight.Score as Gap (PilotDistance(..), PilotTime(..))
import Data.Flight.TrackLog (IxTask(..))
import Flight.Units ()
import Flight.Mask (Masking)
import Flight.Mask.Pilot
    ( checkTracks
    , launched
    , madeGoal
    , distanceToGoal
    , distanceFlown
    , timeFlown
    )
import qualified Data.Flight.PilotTrack as TZ
    ( FlownTrack(..)
    , FlownTrackTag(..)
    , PilotFlownTrack(..)
    , PilotFlownTrackTag(..)
    , MaskedTracks(..)
    , TimedTracks(..)
    , TaskTiming(..)
    , PilotTags(..)
    , Fix(..)
    )
import Data.Number.RoundingFunctions (dpRound)
import Cmd.Inputs (readTags)

driverMain :: IO ()
driverMain = withCmdArgs drive

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        ("timing", _) -> LT
        ("maskedTracks", _) -> GT
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

            let yamlTagsPath =
                    flip replaceExtension ".tag-zone.yaml"
                    $ dropExtension yamlCompPath

            putStrLn $ "Reading competition from '" ++ takeFileName yamlCompPath ++ "'"
            putStrLn $ "Reading tagged zones from '" ++ takeFileName yamlTagsPath ++ "'"
            writeMask yamlTagsPath yamlMaskPath check

            where
                writeMask yamlTagsPath yamlMaskPath f = do
                    comp <-
                        runExceptT $
                            f
                                yamlCompPath
                                (IxTask <$> task)
                                (Cmp.Pilot <$> pilot)

                    tags <- runExceptT $ readTags yamlTagsPath

                    case (comp, tags) of
                        (Left msg, _) -> print msg
                        (_, Left msg) -> print msg
                        (Right comp', Right TZ.PilotTags{pilotTags}) -> do
                            let pss :: [[TZ.PilotFlownTrack]] =
                                    (fmap . fmap)
                                        (\case
                                            Left (p, _) ->
                                                TZ.PilotFlownTrack p Nothing

                                            Right (p, x) ->
                                                TZ.PilotFlownTrack p (Just x))
                                        comp'

                            let zss :: [[TZ.PilotFlownTrack]] =
                                    zipWith
                                        (zipWith merge)
                                        pss
                                        pilotTags

                            let mss :: [TZ.TimedTracks] =
                                    (\zs ->
                                        TZ.TimedTracks
                                            { TZ.timing =
                                                TZ.TaskTiming
                                                    { firstStart = Nothing
                                                    , lastGoal = Nothing
                                                    }
                                            , TZ.maskedTracks = zs
                                            })
                                    <$> zss

                            let tzi =
                                    TZ.MaskedTracks
                                        { masking = mss
                                        }

                            let yaml =
                                    Y.encodePretty
                                        (Y.setConfCompare cmp Y.defConfig)
                                        tzi 

                            BS.writeFile yamlMaskPath yaml

                check =
                    checkTracks $ \Cmp.CompSettings{tasks} -> flown tasks

merge :: TZ.PilotFlownTrack -> TZ.PilotFlownTrackTag -> TZ.PilotFlownTrack

merge track@(TZ.PilotFlownTrack _ Nothing) _ =
    track

merge track (TZ.PilotFlownTrackTag _ Nothing) =
    track

merge
    (TZ.PilotFlownTrack p (Just track))
    (TZ.PilotFlownTrackTag p' (Just TZ.FlownTrackTag{zonesTag})) =
    if p /= p' then error "Merging mismatched pilots" else
    let track' = track { TZ.zonesTime = (fmap . fmap) TZ.time zonesTag }
    in (TZ.PilotFlownTrack p (Just track'))

flown :: Masking TZ.FlownTrack
flown tasks iTask xs =
    let ld = launched tasks iTask xs
        mg = madeGoal tasks iTask xs
        dg = distanceToGoal tasks iTask xs
        df = distanceFlown tasks iTask xs
        tf = timeFlown tasks iTask xs
    in TZ.FlownTrack
        { launched = ld
        , madeGoal = mg
        , zonesTime = []

        , distanceToGoal =
            if mg then Nothing else unTaskDistance <$> dg

        , distanceMade =
            if mg then Nothing else unPilotDistance <$> df

        , timeToGoal =
            if not mg then Nothing else unPilotTime <$> tf
        }
