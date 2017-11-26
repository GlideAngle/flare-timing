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
import Data.Maybe (fromMaybe)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.String (IsString)
import Control.Lens ((^?), element)
import Control.Monad (join, mapM_)
import Control.Applicative.Alternative ((<|>))
import Control.Monad.Except (ExceptT, runExceptT)
import Data.UnitsOfMeasure ((/:), u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (takeFileName, replaceExtension, dropExtension)
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS
import qualified Data.Number.FixedFunctions as F

import Flight.Comp (Pilot(..))
import qualified Flight.Comp as Cmp (CompSettings(..), Task(..))
import qualified Flight.Task as Tsk (TaskDistance(..))
import qualified Flight.Score as Gap (PilotDistance(..), PilotTime(..))
import Flight.TrackLog (IxTask(..), TrackFileFail(..))
import Flight.Units ()
import Flight.Mask
    ( SigMasking
    , TaskZone
    , Ticked(..)
    , checkTracks
    , madeGoal
    , distanceToGoal
    , distanceFlown
    , timeFlown
    , zoneToCylinder
    )
import Flight.Track.Mask (Masking(..), PilotTrackMask(..))
import qualified Flight.Track.Mask as TM (TrackMask(..))
import Flight.Track.Tag (Tagging)
import Flight.Zone (Bearing(..))
import Flight.Zone.Raw (RawZone)
import Flight.LatLng.Rational (Epsilon(..), defEps)
import Data.Number.RoundingFunctions (dpRound)
import Flight.Task (SpanLatLng, CircumSample, AngleCut(..))
import Flight.PointToPoint.Rational
    (distanceHaversine, distancePointToPoint, costSegment)
import Flight.Cylinder.Rational (circumSample)
import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..))
import Cmd.Inputs
    ( MadeGoal(..), ArrivalRank(..)
    , tagMadeGoal, tagArrivalRank
    , readTags
    )

driverMain :: IO ()
driverMain = withCmdArgs drive

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        -- TODO: first start time & last goal time & launched
        ("madeGoal", _) -> LT
        ("arrivalRank", "madeGoal") -> GT
        ("arrivalRank", _) -> LT
        ("timeToGoal", "madeGoal") -> GT
        ("timeToGoal", "arrivalRank") -> GT
        ("timeToGoal", _) -> LT
        ("distanceToGoal", "madeGoal") -> GT
        ("distanceToGoal", "arrivalRank") -> GT
        ("distanceToGoal", "timeToGoal") -> GT
        ("distanceToGoal", _) -> LT
        ("distanceMade", _) -> GT
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
        withFile compPath = do
            let tagPath =
                    flip replaceExtension ".tag-zone.yaml"
                    $ dropExtension compPath

            putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
            putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"

            tags <- runExceptT $ readTags tagPath

            writeMask
                (IxTask <$> task)
                (Pilot <$> pilot)
                compPath
                (check tags)

writeMask :: [IxTask]
          -> [Pilot]
          -> FilePath
          -> (FilePath
              -> [IxTask]
              -> [Pilot]
              -> ExceptT
                   String
                   IO
                   [
                       [Either
                           (Pilot, TrackFileFail)
                           (Pilot, Pilot -> TM.TrackMask)
                       ]
                   ]
             )
          -> IO ()
writeMask selectTasks selectPilots compPath f = do
    checks <- runExceptT $ f compPath selectTasks selectPilots

    case checks of
        Left msg -> print msg
        Right comp -> do
            let pss :: [[PilotTrackMask]] =
                    (fmap . fmap)
                        (\case
                            Left (p, _) ->
                                PilotTrackMask p Nothing

                            Right (p, g) ->
                                PilotTrackMask p (Just (g p)))
                        comp

            let tzi = Masking { masking = pss }

            let yaml =
                    Y.encodePretty
                        (Y.setConfCompare cmp Y.defConfig)
                        tzi 

            BS.writeFile maskPath yaml
    where
        maskPath =
            flip replaceExtension ".mask-track.yaml"
            $ dropExtension compPath

check :: Either String Tagging
      -> FilePath
      -> [IxTask]
      -> [Pilot]
      -> ExceptT
          String
          IO
          [
              [Either
                  (Pilot, TrackFileFail)
                  (Pilot, Pilot -> TM.TrackMask)
              ]
          ]
check tags = checkTracks $ \Cmp.CompSettings{tasks} ->
    (flown tags) tasks

flown :: Either String Tagging -> SigMasking (Pilot -> TM.TrackMask)
flown tags tasks iTask@(IxTask i) xs p =
    TM.TrackMask
        { madeGoal = mg

        , arrivalRank =
            join $ (\f -> f p speedSection' iTask xs) <$> tArrivalRank

        , distanceToGoal =
            if mg then Nothing else fromRational . unTaskDistance <$> dg

        , distanceMade =
            fromRational <$> if mg then Nothing else unPilotDistance <$> df

        , timeToGoal =
            if not mg then Nothing else unPilotTime <$> tf
        }
    where
        dpp = distancePointToPoint
        cseg = costSegment span

        dg =
            distanceToGoal
                noneTicked
                span dpp cseg cs cut
                zoneToCyl tasks iTask xs

        df =
            distanceFlown
                noneTicked
                span dpp cseg cs cut
                zoneToCyl tasks iTask xs

        tf =
            timeFlown
                span
                zoneToCyl tasks iTask xs

        speedSection' =
            case tasks ^? element (fromIntegral i - 1) of
                Nothing -> Nothing
                Just Cmp.Task{..} -> speedSection

        (MadeGoal tMadeGoal) = tagMadeGoal tags
        (ArrivalRank tArrivalRank) = tagArrivalRank tags

        mg =
            let a = join $ (\f -> f p speedSection' iTask xs) <$> tMadeGoal
                b = Just $ madeGoal span zoneToCyl tasks iTask xs

            in fromMaybe False $ a <|> b

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

noneTicked :: Ticked
noneTicked = Ticked 0
