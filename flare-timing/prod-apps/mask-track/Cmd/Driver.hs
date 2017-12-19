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
{-# LANGUAGE TupleSections #-}

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
import Control.Lens ((^?), element)
import Control.Monad (join, mapM_)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.UnitsOfMeasure ((/:), u, convert, toRational', fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.FilePath (takeFileName)
import qualified Data.Number.FixedFunctions as F
import Data.Aeson.ViaScientific (ViaScientific(..))

import Flight.Route (TaskRoutes(..))
import Flight.Comp
    ( Pilot(..)
    , CompInputFile(..)
    , TaskLengthFile(..)
    , TagZoneFile(..)
    , CompSettings(..)
    , Task(..)
    , TrackFileFail(..)
    , compToTaskLength
    , compToCross
    , compToMask
    , crossToTag
    , findCompInput
    )
import Flight.Distance (TaskDistance(..))
import Flight.TrackLog (IxTask(..))
import Flight.Units ()
import qualified Flight.Mask as Mask (distanceToGoal)
import Flight.Mask
    ( Sliver(..), FnIxTask, TaskZone, RaceSections(..)
    , checkTracks, distanceFlown, zoneToCylinder
    )
import Flight.Track.Mask
    (Masking(..), TrackArrival(..), TrackSpeed(..), TrackDistance(..))
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
import Flight.Lookup.Tag
    ( ArrivalRankLookup(..), PilotTimeLookup(..), TickedLookup(..), StartEnd
    , tagArrivalRank, tagPilotTime, tagTicked
    )
import Flight.Scribe (readTagging, readRoute, writeMasking)
import Flight.Lookup.TaskLength (TaskLengthLookup(..), routeLength)
import qualified Flight.Score as Gap (PilotDistance(..), bestTime)
import Flight.Score
    ( PilotsAtEss(..)
    , PositionAtEss(..)
    , BestTime(..)
    , PilotTime(..)
    , arrivalFraction
    , speedFraction
    )
    
type FlightStats = (Maybe (PilotTime, PositionAtEss), Maybe TrackDistance)

driverMain :: IO ()
driverMain = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing
    err <- checkPaths options
    maybe (drive options) putStrLn err

unTaskDistance :: (Real a, Fractional a) => TaskDistance a -> a
unTaskDistance (TaskDistance d) =
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
drive o = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findCompInput o
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    fprint ("Masking tracks completed in " % timeSpecs % "\n") start end

go :: CmdOptions -> CompInputFile -> IO ()
go CmdOptions{..} compFile@(CompInputFile compPath) = do
    let tagFile@(TagZoneFile tagPath) = crossToTag . compToCross $ compFile
    let lenFile@(TaskLengthFile lenPath) = compToTaskLength $ compFile
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading task length from '" ++ takeFileName lenPath ++ "'"
    putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"

    tags <- runExceptT $ readTagging tagFile
    lengths <- runExceptT $ readRoute lenFile

    writeMask
        (IxTask <$> task)
        (Pilot <$> pilot)
        (CompInputFile compPath)
        (check lengths math tags)

writeMask :: [IxTask]
          -> [Pilot]
          -> CompInputFile
          -> (CompInputFile
              -> [IxTask]
              -> [Pilot]
              -> ExceptT
                   String
                   IO
                   [
                       [Either
                           (Pilot, TrackFileFail)
                           (Pilot, Pilot -> FlightStats)
                       ]
                   ]
             )
          -> IO ()
writeMask selectTasks selectPilots compFile f = do
    checks <- runExceptT $ f compFile selectTasks selectPilots

    case checks of
        Left msg -> print msg
        Right comp -> do
            let ys :: [[(Pilot, FlightStats)]] =
                    (fmap . fmap)
                        (\case
                            Left (p, _) -> (p, (Nothing, Nothing))
                            Right (p, g) -> (p, g p))
                        comp

            let d :: [[(Pilot, TrackDistance)]] = distances <$> ys
            let a :: [[(Pilot, TrackArrival)]] = arrivals <$> ys
            let s :: [Maybe (BestTime, [(Pilot, TrackSpeed)])] = times <$> ys

            let maskTrack =
                    Masking
                        { pilotsAtEss =
                            (PilotsAtEss . toInteger . length) <$> a 

                        , bestTime = (fmap . fmap) (ViaScientific . fst) s
                        , arrival = a
                        , speed = (fromMaybe []) <$> (fmap . fmap) snd s
                        , distance = d
                        }

            writeMasking (compToMask compFile) maskTrack

distances :: [(Pilot, FlightStats)] -> [(Pilot, TrackDistance)]
distances xs =
    sortOn (togo . snd) . catMaybes
    $ fmap (\(p, (_, d)) -> (p,) <$> d) xs

arrivals :: [(Pilot, FlightStats)] -> [(Pilot, TrackArrival)]
arrivals xs =
    sortOn (rank . snd) $ (fmap . fmap) f ys
    where
        ys :: [(Pilot, PositionAtEss)]
        ys = catMaybes $ (\(p, (a, _)) -> ((p,) . snd) <$> a) <$> xs

        pilots :: PilotsAtEss
        pilots = PilotsAtEss . toInteger $ length ys

        f position =
            TrackArrival
                { rank = position
                , frac = ViaScientific $ arrivalFraction pilots position
                }

times :: [(Pilot, FlightStats)] -> Maybe (BestTime, [(Pilot, TrackSpeed)])
times xs =
    (\ bt -> (bt, sortOn (time . snd) $ (\(p, t) -> (p, f bt t)) <$> ys))
    <$> Gap.bestTime ts
    where
        ys :: [(Pilot, PilotTime)]
        ys = catMaybes $ (\(p, (a, _)) -> ((p,) . fst) <$> a) <$> xs

        ts :: [PilotTime]
        ts = snd <$> ys

        f best t =
            TrackSpeed
                { time = ViaScientific t
                , frac = ViaScientific $ speedFraction best t
                }

check :: Either String TaskRoutes
      -> Math
      -> Either String Tagging
      -> CompInputFile
      -> [IxTask]
      -> [Pilot]
      -> ExceptT
          String
          IO
          [
              [Either
                  (Pilot, TrackFileFail)
                  (Pilot, Pilot -> FlightStats)
              ]
          ]
check lengths math tags = checkTracks $ \CompSettings{tasks} ->
    flown lengths math tags tasks

flown :: Either String TaskRoutes
      -> Math
      -> Either String Tagging
      -> FnIxTask (Pilot -> FlightStats)
flown routes math tags tasks iTask xs =
    maybe
        (const (Nothing, Nothing))
        (\d -> flown' d math tags tasks iTask xs)
        taskLength
    where
        taskLength = join ((\f -> f iTask) <$> lookupTaskLength)
        (TaskLengthLookup lookupTaskLength) = routeLength routes

flown' :: TaskDistance Double
       -> Math
       -> Either String Tagging
       -> FnIxTask (Pilot -> FlightStats)
flown' dTaskF@(TaskDistance td) math tags tasks iTask@(IxTask i) xs p =
    case tasks ^? element (i - 1) of
        Nothing -> (Nothing, Nothing)
        Just task' ->
            case (pilotTime, arrivalRank) of
                (Nothing, _) -> (Nothing, Just $ distance task')
                (_, Nothing) -> (Nothing, Just $ distance task')
                (Just a, Just b) -> (Just (a, b), Nothing)
    where
        ticked =
            fromMaybe (RaceSections [] [] [])
            $ join ((\f -> f p speedSection' iTask xs) <$> lookupTicked)

        pilotTime =
            diffTimeHours
            <$> join ((\f -> f p speedSection' iTask xs) <$> lookupPilotTime)

        arrivalRank =
            PositionAtEss . toInteger
            <$> join ((\f -> f p speedSection' iTask xs) <$> lookupArrivalRank)

        distance task =
            TrackDistance
                { togo = unTaskDistance <$> dg task math
                , made = fromRational <$> unPilotDistance <$> df task math
                }

        dppR = Rat.distancePointToPoint
        dppF = Dbl.distancePointToPoint

        csegR = Rat.costSegment spanR
        csegF = Dbl.costSegment spanF

        dg :: Task -> Math -> Maybe (TaskDistance Double)
        dg task =
            \case
            Floating ->
                Mask.distanceToGoal
                    ticked
                    (Sliver spanF dppF csegF csF cutF)
                    zoneToCylF task xs

            Rational ->
                (\(TaskDistance d) -> TaskDistance $ fromRational' d) <$>
                Mask.distanceToGoal
                    ticked
                    (Sliver spanR dppR csegR csR cutR)
                    zoneToCylR task xs

        df :: Task -> Math -> Maybe (Gap.PilotDistance Double)
        df task =
            \case
            Floating ->
                distanceFlown
                    dTaskF
                    ticked
                    (Sliver spanF dppF csegF csF cutF)
                    zoneToCylF task xs

            Rational ->
                (\(Gap.PilotDistance d) -> Gap.PilotDistance $ fromRational d) <$>
                distanceFlown
                    dTaskR
                    ticked
                    (Sliver spanR dppR csegR csR cutR)
                    zoneToCylR task xs

        speedSection' =
            case tasks ^? element (fromIntegral i - 1) of
                Nothing -> Nothing
                Just Task{..} -> speedSection

        (TickedLookup lookupTicked) = tagTicked tags
        (ArrivalRankLookup lookupArrivalRank) = tagArrivalRank tags
        (PilotTimeLookup lookupPilotTime) = tagPilotTime tags
        dTaskR = TaskDistance $ toRational' td

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

diffTimeHours :: StartEnd -> PilotTime
diffTimeHours (start, end) =
    PilotTime hours
    where
        secs = toRational $ diffUTCTime end start
        hours = secs * (1 Ratio.% 3600)
