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

import Prelude hiding (last)
import qualified Data.Ratio as Ratio
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Data.Maybe (fromMaybe, catMaybes, isJust)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList, lookup)
import Formatting ((%), fprint)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Lens ((^?), element)
import Control.Monad (join, mapM, zipWithM)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.UnitsOfMeasure ((/:), (-:), u, convert, toRational', fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.FilePath ((</>), takeFileName)
import qualified Data.Number.FixedFunctions as F
import Data.Aeson.ViaScientific (ViaScientific(..))
import Data.Vector (Vector)
import qualified Data.Vector as V (last, null)

import Flight.Comp
    ( Pilot(..)
    , CompInputFile(..)
    , TaskLengthFile(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , CompSettings(..)
    , Task(..)
    , TrackFileFail(..)
    , FlyingSection
    , compToTaskLength
    , compToCross
    , compToMask
    , crossToTag
    , findCompInput
    )
import Flight.Distance (TaskDistance(..))
import Flight.TrackLog (IxTask(..))
import Flight.Units ()
import Flight.Mask
    ( Sliver(..), FnIxTask, TaskZone, RaceSections(..), FlyCut(..)
    , checkTracks, dashDistanceToGoal, dashDistanceFlown, zoneToCylinder
    )
import Flight.Track.Cross (TrackFlyingSection(..))
import Flight.Track.Tag (Tagging)
import Flight.Track.Mask
    ( Masking(..)
    , TrackArrival(..)
    , TrackSpeed(..)
    , TrackDistance(..)
    )
import Flight.Kml (MarkedFixes(..))
import qualified Flight.Track.Time as Time (TickRow(..))
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
import Flight.Lookup.Cross
    (FlyingLookup(..), crossFlying)
import Flight.Lookup.Tag
    ( ArrivalRankLookup(..), TimeLookup(..), TickLookup(..), StartEnd
    , tagArrivalRank, tagPilotTime, tagTicked
    )
import Flight.Scribe
    (readRoute, readCrossing, readTagging, writeMasking, readDiscardFurther)
import Flight.Lookup.Route (RouteLookup(..), routeLength)
import qualified Flight.Score as Gap (PilotDistance(..), bestTime)
import Flight.Score
    ( PilotsAtEss(..)
    , PositionAtEss(..)
    , BestTime(..)
    , PilotTime(..)
    , arrivalFraction
    , speedFraction
    )
import Flight.Comp
    ( DiscardDir(..)
    , AlignTimeFile(..)
    , DiscardFurtherFile(..)
    , discardDir
    , alignPath
    , compFileToCompDir
    )
    
data FlightStats =
    FlightStats
        { statTimeRank :: Maybe (PilotTime, PositionAtEss)
        , statNigh :: Maybe TrackDistance
        , statLand :: Maybe TrackDistance
        }

nullStats :: FlightStats
nullStats =
    FlightStats
        { statTimeRank = Nothing
        , statNigh = Nothing
        , statLand = Nothing
        }

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
    let crossFile@(CrossZoneFile crossPath) = compToCross $ compFile
    let tagFile@(TagZoneFile tagPath) = crossToTag . compToCross $ compFile
    let lenFile@(TaskLengthFile lenPath) = compToTaskLength $ compFile
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading task length from '" ++ takeFileName lenPath ++ "'"
    putStrLn $ "Reading flying time range from '" ++ takeFileName crossPath ++ "'"
    putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"

    crossing <- runExceptT $ readCrossing crossFile
    tagging <- runExceptT $ readTagging tagFile
    routes <- runExceptT $ readRoute lenFile

    let flyingLookup = crossFlying crossing
    let lookupTaskLength = routeLength routes

    case (crossing, tagging, routes) of
        (Left msg, _, _) -> putStrLn msg
        (_, Left msg, _) -> putStrLn msg
        (_, _, Left msg) -> putStrLn msg
        (Right _, Right _, Right _) ->
            writeMask
                lookupTaskLength
                (IxTask <$> task)
                (Pilot <$> pilot)
                (CompInputFile compPath)
                (check math lookupTaskLength flyingLookup tagging)

writeMask :: RouteLookup
          -> [IxTask]
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
writeMask
    (RouteLookup lookupTaskLength)
    selectTasks selectPilots compFile f = do

    checks <- runExceptT $ f compFile selectTasks selectPilots

    case checks of
        Left msg -> print msg
        Right comp -> do
            let ys :: [[(Pilot, FlightStats)]] =
                    (fmap . fmap)
                        (\case
                            Left (p, _) -> (p, nullStats)
                            Right (p, g) -> (p, g p))
                        comp

            let iTasks = IxTask <$> [1 .. length ys]

            -- Distances (ds) of the landout spot.
            let dsLand :: [[(Pilot, TrackDistance)]] = landDistances <$> ys

            -- Arrivals (as).
            let as :: [[(Pilot, TrackArrival)]] = arrivals <$> ys

            -- Velocities (vs).
            let vs :: [Maybe (BestTime, [(Pilot, TrackSpeed)])] = times <$> ys

            -- Times (ts).
            let tsBest = (fmap . fmap) (ViaScientific . fst) vs

            -- For each task, for each pilot, the row closest to goal.
            rows :: [[Maybe (Pilot, Time.TickRow)]]
                    <- readCompBestDistances
                        compFile selectTasks ((fmap . fmap) fst dsLand)

            -- Task lengths (ls).
            let lsTask :: [Maybe (TaskDistance Double)] =
                    (\i -> join ((\g -> g i) <$> lookupTaskLength))
                    <$> iTasks

            let pilotsLandingOut = ((fmap . fmap) fst dsLand)

            -- Distances (ds) of point in the flight closest to goal.
            let dsNigh =
                    zipWith3
                        lookupTaskBestDistance
                        (Map.fromList . catMaybes <$> rows)
                        lsTask
                        pilotsLandingOut

            -- For each task, for each pilot, the best distance made.
            let dsMade :: [Maybe Double] =
                    (\xs -> if null xs then Nothing else Just . maximum $ xs)
                    . catMaybes
                    <$> (fmap . fmap) (made . snd) dsNigh

            -- If a pilot makes goal then their best distance is the task
            -- distance.
            let dsBest :: [Maybe Double] =
                    zipWith3
                        (\l t d -> if isJust t then unTaskDistance <$> l else d)
                        lsTask
                        tsBest
                        dsMade

            writeMasking
                (compToMask compFile)
                Masking
                    { pilotsAtEss = (PilotsAtEss . toInteger . length) <$> as
                    , bestTime = tsBest
                    , taskDistance = (fmap . fmap) unTaskDistance lsTask
                    , bestDistance = dsBest
                    , arrival = as
                    , speed = (fromMaybe []) <$> (fmap . fmap) snd vs
                    , nigh = dsNigh
                    , land = dsLand
                    }

lookupTaskBestDistance
    :: Map Pilot Time.TickRow
    -> Maybe (TaskDistance Double)
    -> [Pilot]
    -> [(Pilot, TrackDistance)]
lookupTaskBestDistance m td =
    sortOn (togo . snd)
    . catMaybes
    . fmap (lookupPilotBestDistance m td)

lookupPilotBestDistance
    :: Map Pilot Time.TickRow
    -> Maybe (TaskDistance Double)
    -> Pilot
    -> Maybe (Pilot, TrackDistance)
lookupPilotBestDistance m td p =
    ((p,) . madeDistance td) <$> (Map.lookup p m)

madeDistance
    :: Maybe (TaskDistance Double)
    -> Time.TickRow
    -> TrackDistance

madeDistance Nothing Time.TickRow{distance} =
    TrackDistance
        { togo = Just distance
        , made = Nothing
        }

madeDistance (Just (TaskDistance td)) Time.TickRow{distance} =
    TrackDistance
        { togo = Just distance
        , made = Just . unTaskDistance . TaskDistance $ td -: togo'
        }
    where
        togo :: Quantity Double [u| km |]
        togo = MkQuantity distance

        togo' = convert togo :: Quantity Double [u| m |]

readCompBestDistances
    :: CompInputFile
    -> [IxTask]
    -> [[Pilot]]
    -> IO [[Maybe (Pilot, Time.TickRow)]]
readCompBestDistances compFile selectTasks pss =
    zipWithM
        (\ i ps ->
            if not (includeTask selectTasks i)
               then return []
               else readTaskBestDistances compFile i ps)
        (IxTask <$> [1 .. ])
        pss

readTaskBestDistances
    :: CompInputFile
    -> IxTask
    -> [Pilot]
    -> IO [Maybe (Pilot, Time.TickRow)]
readTaskBestDistances compFile i ps =
    mapM (readPilotBestDistance compFile i) ps

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)

readPilotBestDistance
    :: CompInputFile
    -> IxTask
    -> Pilot
    -> IO (Maybe (Pilot, Time.TickRow))
readPilotBestDistance compFile (IxTask iTask) pilot = do
    rows <-
        runExceptT
        $ readDiscardFurther (DiscardFurtherFile (dOut </> file))

    return $ (pilot,) <$> either (const Nothing) (lastRow . snd) rows
    where
        dir = compFileToCompDir compFile
        (_, AlignTimeFile file) = alignPath dir iTask pilot
        (DiscardDir dOut) = discardDir dir iTask

lastRow :: Vector Time.TickRow -> Maybe Time.TickRow
lastRow xs =
    if V.null xs then Nothing else Just $ V.last xs

landDistances :: [(Pilot, FlightStats)] -> [(Pilot, TrackDistance)]
landDistances xs =
    sortOn (togo . snd)
    . catMaybes
    $ fmap (\(p, FlightStats{..}) -> (p,) <$> statLand) xs

arrivals :: [(Pilot, FlightStats)] -> [(Pilot, TrackArrival)]
arrivals xs =
    sortOn (rank . snd) $ (fmap . fmap) f ys
    where
        ys :: [(Pilot, PositionAtEss)]
        ys =
            catMaybes
            $ (\(p, FlightStats{..}) -> ((p,) . snd) <$> statTimeRank)
            <$> xs

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
        ys =
            catMaybes
            $ (\(p, FlightStats{..}) -> ((p,) . fst) <$> statTimeRank)
            <$> xs

        ts :: [PilotTime]
        ts = snd <$> ys

        f best t =
            TrackSpeed
                { time = ViaScientific t
                , frac = ViaScientific $ speedFraction best t
                }

check :: Math
      -> RouteLookup
      -> FlyingLookup
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
check math lengths flying tags = checkTracks $ \CompSettings{tasks} ->
    flown math lengths flying tags tasks

flown :: Math
      -> RouteLookup
      -> FlyingLookup
      -> Either String Tagging
      -> FnIxTask (Pilot -> FlightStats)
flown
    math
    (RouteLookup lookupTaskLength)
    flying
    tags tasks iTask fixes =
    maybe
        (const $ FlightStats Nothing Nothing Nothing)
        (\d -> flown' d flying math tags tasks iTask fixes)
        taskLength
    where
        taskLength = join ((\f -> f iTask) <$> lookupTaskLength)

flown' :: TaskDistance Double
       -> FlyingLookup
       -> Math
       -> Either String Tagging
       -> FnIxTask (Pilot -> FlightStats)
flown'
    dTaskF@(TaskDistance td)
    (FlyingLookup lookupFlying)
    math tags tasks iTask@(IxTask i)
    mf@MarkedFixes{mark0}
    p =
    case tasks ^? element (i - 1) of
        Nothing -> nullStats

        Just task' ->
            case (pilotTime, arrivalRank) of
                (Nothing, _) ->
                    nullStats {statLand = Just $ landDistance task' }

                (_, Nothing) ->
                    nullStats {statLand = Just $ landDistance task' }

                (Just a, Just b) ->
                    nullStats {statTimeRank = Just (a, b)}
    where
        flyingRange :: FlyingSection UTCTime =
            fromMaybe (Just (mark0, mark0))
            $ join (fmap flyingTimes . (\f -> f iTask p) <$> lookupFlying)

        xs =
            FlyCut
                { cut = flyingRange
                , uncut = mf
                }

        ticked =
            fromMaybe (RaceSections [] [] [])
            $ join ((\f -> f iTask speedSection' p mf) <$> lookupTicked)

        pilotTime =
            diffTimeHours
            <$> join ((\f -> f iTask speedSection' p mf) <$> lookupPilotTime)

        arrivalRank =
            PositionAtEss . toInteger
            <$> join ((\f -> f iTask speedSection' p mf) <$> lookupArrivalRank)

        landDistance task =
                TrackDistance
                    { togo = unTaskDistance <$> dgLast task math
                    , made = fromRational <$> unPilotDistance <$> dfLast task math
                    }

        dppR = Rat.distancePointToPoint
        dppF = Dbl.distancePointToPoint

        csegR = Rat.costSegment spanR
        csegF = Dbl.costSegment spanF

        dgLast :: Task -> Math -> Maybe (TaskDistance Double)
        dgLast task =
            \case
            Floating ->
                dashDistanceToGoal
                    ticked
                    (Sliver spanF dppF csegF csF cutF)
                    zoneToCylF task xs

            Rational ->
                (\(TaskDistance d) -> TaskDistance $ fromRational' d) <$>
                dashDistanceToGoal
                    ticked
                    (Sliver spanR dppR csegR csR cutR)
                    zoneToCylR task xs

        dfLast :: Task -> Math -> Maybe (Gap.PilotDistance Double)
        dfLast task =
            \case
            Floating ->
                dashDistanceFlown
                    dTaskF
                    ticked
                    (Sliver spanF dppF csegF csF cutF)
                    zoneToCylF task xs

            Rational ->
                (\(Gap.PilotDistance d) -> Gap.PilotDistance $ fromRational d) <$>
                dashDistanceFlown
                    dTaskR
                    ticked
                    (Sliver spanR dppR csegR csR cutR)
                    zoneToCylR task xs

        speedSection' =
            case tasks ^? element (fromIntegral i - 1) of
                Nothing -> Nothing
                Just Task{..} -> speedSection

        (TickLookup lookupTicked) = tagTicked tags
        (ArrivalRankLookup lookupArrivalRank) = tagArrivalRank tags
        (TimeLookup lookupPilotTime) = tagPilotTime tags
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
