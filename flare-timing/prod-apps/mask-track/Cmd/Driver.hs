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

import qualified Data.Ratio as Ratio
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (sortOn)
import Formatting ((%), fprint)
import Data.Time.Clock (diffUTCTime)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.String (IsString)
import Control.Lens ((^?), element)
import Control.Monad (join, mapM_)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.UnitsOfMeasure ((/:), u, convert, toRational', fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find
    (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (takeFileName, replaceExtension, dropExtension)
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS
import qualified Data.Number.FixedFunctions as F
import Data.Aeson.ViaScientific (ViaScientific(..))

import Flight.Comp
    ( Pilot(..)
    , CompFile(..)
    , TagFile(..)
    , CompSettings(..)
    , Task(..)
    , TrackFileFail(..)
    , compToCross
    , crossToTag
    )
import qualified Flight.Task as Tsk (TaskDistance(..))
import Flight.TrackLog (IxTask(..))
import Flight.Units ()
import Flight.Mask
    ( SigMasking, TaskZone, Ticked(..)
    , checkTracks, distanceToGoal, distanceFlown, zoneToCylinder
    )
import Flight.Track.Mask
    (Masking(..), PilotTrackMask(..), TrackArrival(..), TrackSpeed(..))
import qualified Flight.Track.Mask as TM (TrackMask(..))
import Flight.Track.Tag (Tagging)
import Flight.Zone (Bearing(..))
import Flight.Zone.Raw (RawZone)
import Flight.LatLng.Rational (Epsilon(..), defEps)
import Data.Number.RoundingFunctions (dpRound)
import Flight.Task (SpanLatLng, CircumSample, AngleCut(..))
import qualified Flight.PointToPoint.Rational as Rat
    (distanceHaversine, distancePointToPoint, costSegment)
import qualified Flight.PointToPoint.Double as Dbl
    (distanceHaversine, distancePointToPoint, costSegment)
import qualified Flight.Cylinder.Rational as Rat (circumSample)
import qualified Flight.Cylinder.Double as Dbl (circumSample)

import Flight.Cmd.Paths (checkPaths)
import Flight.Cmd.Options (Math(..), CmdOptions(..), ProgramName(..), mkOptions)
import Cmd.Options (description)
import Cmd.Inputs
    ( ArrivalRankLookup(..), PilotTimeLookup(..), StartEnd
    , tagArrivalRank, tagPilotTime , readTags
    )
import qualified Flight.Score as Gap (PilotDistance(..), bestTime)
import Flight.Score
    ( PilotsAtEss(..)
    , PositionAtEss(..)
    , BestTime(..)
    , PilotTime(..)
    , arrivalFraction
    , speedFraction
    )

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
        -- TODO: first start time & last goal time & launched
        ("pilotsAtEss", _) -> LT

        ("bestTime", "pilotsAtEss") -> GT
        ("bestTime", _) -> LT

        ("arrival", "pilotsAtEss") -> GT
        ("arrival", "bestTime") -> GT
        ("arrival", _) -> LT

        ("speed", "pilotsAtEss") -> GT
        ("speed", "bestTime") -> GT
        ("speed", "arrival") -> GT
        ("speed", _) -> LT

        ("masking", _) -> GT

        ("time", _) -> LT
        ("rank", _) -> LT
        ("frac", _) -> GT

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

drive :: CmdOptions -> IO ()
drive CmdOptions{..} = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    dfe <- doesFileExist file
    if dfe then
        withFile (CompFile file)
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- find always (fileType ==? RegularFile &&? extension ==? ".comp-inputs.yaml") dir
            mapM_ withFile (CompFile <$> files)
        else
            putStrLn "Couldn't find any flight score competition yaml input files."
    end <- getTime Monotonic
    fprint ("Masking tracks completed in " % timeSpecs % "\n") start end
    where
        withFile compFile@(CompFile compPath) = do
            let tagFile@(TagFile tagPath) = crossToTag . compToCross $ compFile
            putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
            putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"

            tags <- runExceptT $ readTags tagFile

            writeMask
                (IxTask <$> task)
                (Pilot <$> pilot)
                (CompFile compPath)
                (check math tags)

writeMask :: [IxTask]
          -> [Pilot]
          -> CompFile
          -> (CompFile
              -> [IxTask]
              -> [Pilot]
              -> ExceptT
                   String
                   IO
                   [
                       [Either
                           (Pilot, TrackFileFail)
                           (Pilot, Pilot ->
                               (Maybe (PilotTime, PositionAtEss), TM.TrackMask))
                       ]
                   ]
             )
          -> IO ()
writeMask selectTasks selectPilots compFile@(CompFile compPath) f = do
    checks <- runExceptT $ f compFile selectTasks selectPilots

    case checks of
        Left msg -> print msg
        Right comp -> do
            let ys :: [([(Pilot, Maybe (PilotTime, PositionAtEss))], [PilotTrackMask])] =
                    unzip <$>
                    (fmap . fmap)
                        (\case
                            Left (p, _) ->
                                ((p, Nothing), PilotTrackMask p Nothing)

                            Right (p, g) ->
                                let (ar, tm) = g p
                                in ((p, ar), PilotTrackMask p (Just tm)))
                        comp

            let as :: [[(Pilot, TrackArrival)]] =
                    (arrivals . pilotMay) <$> (fst <$> ys)

            let ts :: [Maybe (BestTime, [(Pilot, TrackSpeed)])] =
                    (times . pilotMay) <$> (fst <$> ys)

            let tzi =
                    Masking
                        { pilotsAtEss =
                            (PilotsAtEss . toInteger . length) <$> as 

                        , bestTime = (fmap . fmap) (ViaScientific . fst) ts
                        , arrival = as
                        , speed = (fromMaybe []) <$> (fmap . fmap) snd ts
                        , masking = snd <$> ys
                        }

            let yaml =
                    Y.encodePretty
                        (Y.setConfCompare cmp Y.defConfig)
                        tzi 

            BS.writeFile maskPath yaml
    where
        maskPath =
            flip replaceExtension ".mask-track.yaml"
            $ dropExtension compPath

pilotMay :: [(Pilot, Maybe a)] -> [(Pilot, a)]
pilotMay xs =
    catMaybes $ f <$> xs
    where
        f (_, Nothing) = Nothing
        f (p, Just e) = Just (p, e)

arrivals :: [(Pilot, (PilotTime, PositionAtEss))]
         -> [(Pilot, TrackArrival)]
arrivals xs =
    sortOn (rank . snd)
    $ (fmap . fmap) ((f (PilotsAtEss . toInteger $ length xs)) . snd) xs
    where
        f pilots position =
            TrackArrival
                { rank = position
                , frac = ViaScientific $ arrivalFraction pilots position
                }

times :: [(Pilot, (PilotTime, PositionAtEss))]
      -> Maybe (BestTime, [(Pilot, TrackSpeed)])
times xs =
    (\ bt -> (bt, sortOn (time . snd) $ (fmap . fmap) ((f bt) . fst) xs))
    <$> Gap.bestTime (fst . snd <$> xs)
    where
        f best t =
            TrackSpeed
                { time = ViaScientific t
                , frac = ViaScientific $ speedFraction best t
                }

check :: Math
      -> Either String Tagging
      -> CompFile
      -> [IxTask]
      -> [Pilot]
      -> ExceptT
          String
          IO
          [
              [Either
                  (Pilot, TrackFileFail)
                  (Pilot, Pilot -> (Maybe (PilotTime, PositionAtEss), TM.TrackMask))
              ]
          ]
check math tags = checkTracks $ \CompSettings{tasks} ->
    flown math tags tasks

flown :: Math
      -> Either String Tagging
      -> SigMasking (Pilot -> (Maybe (PilotTime, PositionAtEss), TM.TrackMask))
flown math tags tasks iTask@(IxTask i) xs p =
    case (pilotTime, arrivalRank) of
        (Nothing, _) -> (Nothing, trackMask)
        (_, Nothing) -> (Nothing, trackMask)
        (Just a, Just b) -> (Just (a, b), trackMask)
    where
        pilotTime =
            diffTimeHours
            <$> join ((\f -> f p speedSection' iTask xs) <$> lookupPilotTime)

        arrivalRank =
            PositionAtEss . toInteger
            <$> join ((\f -> f p speedSection' iTask xs) <$> lookupArrivalRank)

        trackMask =
            TM.TrackMask
                { distanceToGoal = unTaskDistance <$> dg math
                , distanceMade = fromRational <$> unPilotDistance <$> df math
                }

        dppR = Rat.distancePointToPoint
        dppF = Dbl.distancePointToPoint

        csegR = Rat.costSegment spanR
        csegF = Dbl.costSegment spanF

        dg :: Math -> Maybe (Tsk.TaskDistance Double)
        dg =
            \case
            Floating ->
                distanceToGoal
                    noneTicked
                    spanF dppF csegF csF cutF
                    zoneToCylF tasks iTask xs

            Rational ->
                (\(Tsk.TaskDistance d) -> Tsk.TaskDistance $ fromRational' d) <$>
                distanceToGoal
                    noneTicked
                    spanR dppR csegR csR cutR
                    zoneToCylR tasks iTask xs

        df :: Math -> Maybe (Gap.PilotDistance Double)
        df =
            \case
            Floating ->
                distanceFlown
                    noneTicked
                    spanF dppF csegF csF cutF
                    zoneToCylF tasks iTask xs

            Rational ->
                (\(Gap.PilotDistance d) -> Gap.PilotDistance $ fromRational d) <$>
                distanceFlown
                    noneTicked
                    spanR dppR csegR csR cutR
                    zoneToCylR tasks iTask xs

        speedSection' =
            case tasks ^? element (fromIntegral i - 1) of
                Nothing -> Nothing
                Just Task{..} -> speedSection

        (ArrivalRankLookup lookupArrivalRank) = tagArrivalRank tags
        (PilotTimeLookup lookupPilotTime) = tagPilotTime tags

zoneToCylR :: RawZone -> TaskZone Rational
zoneToCylR = zoneToCylinder

zoneToCylF :: RawZone -> TaskZone Double
zoneToCylF = zoneToCylinder

spanR :: SpanLatLng Rational
spanR = Rat.distanceHaversine defEps

spanF :: SpanLatLng Double
spanF = Dbl.distanceHaversine

csR :: CircumSample Rational
csR = Rat.circumSample

csF :: CircumSample Double
csF = Dbl.circumSample

cutR :: AngleCut Rational
cutR =
    AngleCut
        { sweep = let (Epsilon e) = defEps in Bearing . MkQuantity $ F.pi e
        , nextSweep = nextCutR
        }

cutF :: AngleCut Double
cutF =
    AngleCut
        { sweep = Bearing $ MkQuantity pi
        , nextSweep = nextCutF
        }

nextCutR :: AngleCut Rational -> AngleCut Rational
nextCutR x@AngleCut{sweep} =
    let (Bearing b) = sweep in x{sweep = Bearing $ b /: 2}

nextCutF :: AngleCut Double -> AngleCut Double
nextCutF x@AngleCut{sweep} =
    let (Bearing b) = sweep in x{sweep = Bearing $ b /: 2}

noneTicked :: Ticked
noneTicked = Ticked 0

diffTimeHours :: StartEnd -> PilotTime
diffTimeHours (start, end) =
    PilotTime hours
    where
        secs = toRational $ diffUTCTime end start
        hours = secs * (1 Ratio.% 3600)
