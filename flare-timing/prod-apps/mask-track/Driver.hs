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
{-# LANGUAGE ParallelListComp #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Driver (driverMain) where

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Data.Maybe (fromMaybe, catMaybes, isJust)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList, lookup)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Arrow (second)
import Control.Lens ((^?), element)
import Control.Monad (join)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.UnitsOfMeasure ((-:), u, convert, toRational', fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.FilePath (takeFileName)

import qualified Flight.Comp as Cmp (Nominal(..))
import Flight.Comp
    ( CompInputFile(..)
    , TaskLengthFile(..)
    , CrossZoneFile(..)
    , TagZoneFile(..)
    , CompSettings(..)
    , Pilot(..)
    , Task(..)
    , IxTask(..)
    , TrackFileFail(..)
    , RouteLookup(..)
    , compToTaskLength
    , compToCross
    , compToMask
    , crossToTag
    , findCompInput
    , speedSectionToLeg
    )
import Flight.Distance (TaskDistance(..))
import Flight.Units ()
import Flight.Mask
    ( Sliver(..), FnIxTask, FlyCut(..)
    , checkTracks
    , dashPathToGoalTimeRows
    , dashDistanceToGoal
    , dashDistanceFlown
    )
import Flight.Track.Tag (Tagging)
import Flight.Track.Time (LeadTick(..))
import qualified Flight.Track.Time as Time (TimeRow(..), TickRow(..))
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Distance (TrackDistance(..))
import Flight.Track.Lead (compLeading)
import Flight.Track.Mask (Masking(..), Nigh, Land)
import Flight.Track.Speed (TrackSpeed(..))
import Flight.Kml (MarkedFixes(..))
import Data.Number.RoundingFunctions (dpRound)
import Flight.Task (fromZs)
import Flight.Cmd.Paths (checkPaths)
import Flight.Cmd.Options (Math(..), CmdOptions(..), ProgramName(..), mkOptions)
import Flight.Lookup.Cross (FlyingLookup(..), crossFlying)
import qualified Flight.Lookup as Lookup
    (flyingTimeRange, arrivalRank, pilotTime, ticked, compRoutes, compRaceTimes)
import Flight.Lookup.Tag
    ( TaskTimeLookup(..)
    , tagTaskTime
    , tagArrivalRank
    , tagPilotTime
    , tagTicked
    )
import Flight.Scribe
    ( readComp, readRoute, readCrossing, readTagging, writeMasking
    , readCompLeading, readCompBestDistances, readCompTimeRows
    )
import Flight.Lookup.Route (routeLength)
import qualified Flight.Score as Gap (PilotDistance(..), bestTime)
import Flight.Score
    ( PilotsAtEss(..)
    , PositionAtEss(..)
    , BestTime(..)
    , PilotTime(..)
    , NominalDistance(..)
    , arrivalFraction
    , speedFraction
    )
import Flight.Route (ToTrackLine(..), TrackLine(..))
import Flight.TaskTrack.Double ()
import Span.Double (zoneToCylF, spanF, csF, cutF, dppF, csegF)
import Span.Rational (zoneToCylR, spanR, csR, cutR, dppR, csegR)
import Options (description)
import Stats (FlightStats(..), DashPathInputs(..), nullStats)
    
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
    let lenFile@(TaskLengthFile lenPath) = compToTaskLength compFile
    let crossFile@(CrossZoneFile crossPath) = compToCross compFile
    let tagFile@(TagZoneFile tagPath) = crossToTag . compToCross $ compFile
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading task length from '" ++ takeFileName lenPath ++ "'"
    putStrLn $ "Reading flying time range from '" ++ takeFileName crossPath ++ "'"
    putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"

    compSettings <- runExceptT $ readComp compFile
    crossing <- runExceptT $ readCrossing crossFile
    tagging <- runExceptT $ readTagging tagFile
    routes <- runExceptT $ readRoute lenFile

    let flyingLookup = crossFlying crossing
    let lookupTaskLength = routeLength routes

    case (compSettings, crossing, tagging, routes) of
        (Left msg, _, _, _) -> putStrLn msg
        (_, Left msg, _, _) -> putStrLn msg
        (_, _, Left msg, _) -> putStrLn msg
        (_, _, _, Left msg) -> putStrLn msg
        (Right cs, Right _, Right _, Right _) ->
            writeMask
                cs
                lookupTaskLength
                (tagTaskTime tagging)
                (IxTask <$> task)
                (Pilot <$> pilot)
                compFile
                (check math lookupTaskLength flyingLookup tagging)

writeMask
    :: CompSettings
    -> RouteLookup
    -> TaskTimeLookup
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
    CompSettings
        { nominal =
            Cmp.Nominal
                { distance = NominalDistance (MkQuantity dNom)
                }
        , tasks
        }
    routes
    lookupTaskTime
    selectTasks selectPilots compFile f = do

    checks <- runExceptT $ f compFile selectTasks selectPilots

    case checks of
        Left msg -> print msg
        Right flights -> do
            let ys :: [[(Pilot, FlightStats)]] =
                    (fmap . fmap)
                        (\case
                            Left (p, _) -> (p, nullStats)
                            Right (p, g) -> (p, g p))
                        flights

            let iTasks = IxTask <$> [1 .. length ys]

            -- Zones (zs) of the task and zones ticked.
            let zsTaskTicked :: [Map Pilot DashPathInputs] =
                    Map.fromList . landTaskTicked <$> ys

            -- Distances (ds) of the landout spot.
            let dsLand :: [[(Pilot, TrackDistance Land)]] = landDistances <$> ys

            -- Arrivals (as).
            let as :: [[(Pilot, TrackArrival)]] = arrivals <$> ys

            -- Velocities (vs).
            let vs :: [Maybe (BestTime (Quantity Double [u| h |]), [(Pilot, TrackSpeed)])] =
                    times <$> ys

            -- Times (ts).
            let tsBest = (fmap . fmap) fst vs

            -- For each task, for each pilot, the row closest to goal.
            rows :: [[Maybe (Pilot, Time.TickRow)]]
                <- readCompBestDistances
                    compFile
                    (includeTask selectTasks)
                    ((fmap . fmap) fst dsLand)

            -- Task lengths (ls).
            let lsTask = Lookup.compRoutes routes iTasks

            let pilotsArriving = (fmap . fmap) fst as 
            let pilotsLandingOut = (fmap . fmap) fst dsLand
            let pilots =
                    [ pAs ++ pLs
                    | pAs <- pilotsArriving
                    | pLs <- pilotsLandingOut
                    ]

            let raceTime = Lookup.compRaceTimes lookupTaskTime iTasks tasks

            rowsLeadingStep :: [[(Pilot, [Time.TickRow])]]
                <- readCompLeading
                        routes compFile (includeTask selectTasks)
                        (IxTask <$> [1 .. ])
                        (speedSectionToLeg . speedSection <$> tasks)
                        raceTime
                        pilots

            let (minLead, lead) = compLeading rowsLeadingStep lsTask tasks

            -- Distances (ds) of point in the flight closest to goal.
            let dsNigh :: [[(Pilot, TrackDistance Land)]] =
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

            let dsSum :: [Maybe Double] =
                    [ \case [] -> Nothing; xs -> Just . sum $ xs
                      $ (\d -> max 0 (d - dNom))
                      <$> ds
                    | ds <- catMaybes <$> (fmap . fmap) (made . snd) dsNigh
                    ]

            let rowTicks :: [[Maybe (Pilot, Maybe LeadTick)]] =
                    (fmap . fmap . fmap)
                        (fmap (\Time.TickRow{tickLead} -> tickLead))
                        rows

            dsNighRows :: [[Maybe (Pilot, Time.TimeRow)]]
                    <- readCompTimeRows
                        compFile
                        (includeTask selectTasks)
                        (catMaybes <$> rowTicks)

            let dsNighRows' :: [[(Pilot, TrackDistance Nigh)]] =
                    [ nighTrackLine td zs <$> xs
                    | td <- lsTask
                    | zs <- zsTaskTicked
                    | xs <- (catMaybes <$> dsNighRows)
                    ]

            writeMasking
                (compToMask compFile)
                Masking
                    { pilotsAtEss = (PilotsAtEss . toInteger . length) <$> as
                    , raceTime = raceTime
                    , bestTime = tsBest
                    , taskDistance = (fmap . fmap) unTaskDistance lsTask
                    , bestDistance = dsBest
                    , sumDistance = dsSum
                    , minLead = minLead
                    , lead = lead
                    , arrival = as
                    , speed = fromMaybe [] <$> (fmap . fmap) snd vs
                    , nigh = dsNighRows'
                    , land = dsLand
                    }

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)

nighTrackLine
    :: Maybe (TaskDistance Double)
    -> Map Pilot DashPathInputs
    -> (Pilot, Time.TimeRow)
    -> (Pilot, TrackDistance Nigh)

nighTrackLine Nothing _ (p, Time.TimeRow{distance}) =
    (p,) TrackDistance
        { togo = Just $ distanceOnlyLine distance
        , made = Nothing
        }

nighTrackLine (Just (TaskDistance td)) zsTaskTicked (p, row@Time.TimeRow{distance}) =
    (p,) TrackDistance
        { togo = Just line
        , made = Just . unTaskDistance . TaskDistance $ td -: mTogo
        }
    where
        kmTogo :: Quantity Double [u| km |]
        kmTogo = MkQuantity distance

        mTogo = convert kmTogo :: Quantity Double [u| m |]

        line =
            case Map.lookup p zsTaskTicked of
                Nothing -> distanceOnlyLine distance
                Just dpi -> pathToGo dpi row distance

distanceOnlyLine :: Double -> TrackLine
distanceOnlyLine d =
    TrackLine
        { distance = d
        , waypoints = []
        , legs = []
        , legsSum = []
        }

pathToGo :: DashPathInputs -> Time.TimeRow -> Double -> TrackLine
pathToGo DashPathInputs{..} x@Time.TimeRow{time} d =
    case dashTask of
        Nothing -> distanceOnlyLine d
        Just dashTask' ->
            maybe
                (distanceOnlyLine d)
                (toTrackLine False)
                (fromZs path)
            where
                path = dashPathToGoalTimeRows
                        dashTicked
                        (Sliver spanF dppF csegF csF cutF)
                        zoneToCylF dashTask'
                        FlyCut{cut = Just (time, time), uncut = [x]}

lookupTaskBestDistance
    :: Map Pilot Time.TickRow
    -> Maybe (TaskDistance Double)
    -> [Pilot]
    -> [(Pilot, TrackDistance Land)]
lookupTaskBestDistance m td =
    sortOn (togo . snd)
    . catMaybes
    . fmap (lookupPilotBestDistance m td)

lookupPilotBestDistance
    :: Map Pilot Time.TickRow
    -> Maybe (TaskDistance Double)
    -> Pilot
    -> Maybe (Pilot, TrackDistance Land)
lookupPilotBestDistance m td p =
    ((p,) . madeDistance td) <$> Map.lookup p m

madeDistance
    :: Maybe (TaskDistance Double)
    -> Time.TickRow
    -> TrackDistance Land

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

landTaskTicked :: [(Pilot, FlightStats)] -> [(Pilot, DashPathInputs)]
landTaskTicked xs =
    (\(p, FlightStats{..}) -> (p, statDash)) <$> xs

landDistances :: [(Pilot, FlightStats)] -> [(Pilot, TrackDistance Land)]
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
                , frac = arrivalFraction pilots position
                }

times
    :: [(Pilot, FlightStats)]
    -> Maybe (BestTime (Quantity Double [u| h |]), [(Pilot, TrackSpeed)])
times xs =
    (\ bt -> (bt, sortOn (time . snd) $ second (f bt) <$> ys))
    <$> Gap.bestTime ts
    where
        ys :: [(Pilot, PilotTime (Quantity Double [u| h |]))]
        ys =
            catMaybes
            $ (\(p, FlightStats{..}) -> ((p,) . fst) <$> statTimeRank)
            <$> xs

        ts :: [PilotTime (Quantity Double [u| h |])]
        ts = snd <$> ys

        f best t =
            TrackSpeed
                { time = t
                , frac = speedFraction best t
                }

check
    :: Math
    -> RouteLookup
    -> FlyingLookup
    -> Either String Tagging
    -> CompInputFile
    -> [IxTask]
    -> [Pilot]
    -> ExceptT
        String
        IO
        [[Either (Pilot, TrackFileFail) (Pilot, Pilot -> FlightStats)]]
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
        (const nullStats)
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
    flying
    math tags tasks iTask@(IxTask i)
    mf@MarkedFixes{mark0}
    p =
    case maybeTask of
        Nothing -> nullStats

        Just task' ->
            case (pilotTime', arrivalRank) of
                (Nothing, _) ->
                    tickedStats {statLand = Just $ landDistance task' }

                (_, Nothing) ->
                    tickedStats {statLand = Just $ landDistance task' }

                (Just a, Just b) ->
                    tickedStats {statTimeRank = Just (a, b)}
    where
        maybeTask = tasks ^? element (i - 1)

        ticked = Lookup.ticked (tagTicked tags) mf iTask speedSection' p
        pilotTime' = Lookup.pilotTime (tagPilotTime tags) mf iTask speedSection' p
        arrivalRank = Lookup.arrivalRank (tagArrivalRank tags) mf iTask speedSection' p

        xs =
            FlyCut
                { cut = Lookup.flyingTimeRange flying mark0 iTask p
                , uncut = mf
                }


        tickedStats =
            nullStats
                { statDash =
                    DashPathInputs
                        { dashTask = maybeTask
                        , dashTicked = ticked
                        , dashFlyCut = Just xs
                        }
                }

        landDistance task =
                TrackDistance
                    { togo = unTaskDistance <$> dgLast task math
                    , made = fromRational . unPilotDistance <$> dfLast task math
                    }

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

        dTaskR = TaskDistance $ toRational' td
