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

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Arrow (second)
import Control.Lens ((^?), element)
import Control.Monad (join)
import Control.Applicative (liftA2)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.FilePath (takeFileName)

import qualified Flight.Comp as Cmp (Nominal(..))
import Flight.Comp
    ( FileType(CompInput)
    , CompInputFile(..)
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
    , ensureExt
    )
import Flight.Distance (TaskDistance(..), unTaskDistance)
import Flight.Units ()
import Flight.Mask
    ( FnIxTask, FlyCut(..)
    , checkTracks
    , togoAtLanding
    , madeAtLanding
    )
import Flight.Comp.Distance (compDistance, compNigh)
import Flight.Track.Tag (Tagging)
import qualified Flight.Track.Time as Time (TimeRow(..), TickRow(..))
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Distance (TrackDistance(..), Land)
import Flight.Track.Lead (compLeading)
import Flight.Track.Mask (Masking(..))
import Flight.Track.Speed (TrackSpeed(..))
import Flight.Kml (MarkedFixes(..))
import Data.Ratio.Rounding (dpRound)
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (CmdOptions(..), ProgramName(..), mkOptions)
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
import qualified Flight.Score as Gap (PilotDistance(..), bestTime')
import Flight.Score
    ( PilotsAtEss(..), PositionAtEss(..), BestTime(..), PilotTime(..)
    , arrivalFraction, speedFraction
    )
import Flight.Span.Math (Math(..))
import MaskTrackOptions (description)
import Stats (TimeStats(..), FlightStats(..), DashPathInputs(..), nullStats)
    
main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = ensureExt CompInput}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

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
        { nominal = Cmp.Nominal{free}
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
            let ssVs :: [Maybe (BestTime (Quantity Double [u| h |]), [(Pilot, TrackSpeed)])] =
                    times ssTime <$> ys

            let gsVs :: [Maybe (BestTime (Quantity Double [u| h |]), [(Pilot, TrackSpeed)])] =
                    times gsTime <$> ys

            -- Times (ts).
            let ssBestTime = (fmap . fmap) fst ssVs
            let gsBestTime = (fmap . fmap) fst gsVs

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

            let (dsSumArriving, dsSumLandingOut, dsBest, rowTicks) =
                    compDistance
                        free
                        lsTask
                        pilotsArriving
                        pilotsLandingOut
                        gsBestTime
                        rows

            let dsSum =
                    [ liftA2 (+) aSum lSum
                    | aSum <- dsSumArriving
                    | lSum <- dsSumLandingOut
                    ]

            dsNighRows :: [[Maybe (Pilot, Time.TimeRow)]]
                <- readCompTimeRows
                        compFile
                        (includeTask selectTasks)
                        (catMaybes <$> rowTicks)

            let dsNigh = compNigh lsTask zsTaskTicked dsNighRows

            writeMasking
                (compToMask compFile)
                Masking
                    { pilotsAtEss = (PilotsAtEss . toInteger . length) <$> as
                    , raceTime = raceTime
                    , ssBestTime = ssBestTime
                    , gsBestTime = gsBestTime
                    , taskDistance = (fmap . fmap) unTaskDistance lsTask
                    , bestDistance = dsBest
                    , sumDistance = dsSum
                    , minLead = minLead
                    , lead = lead
                    , arrival = as
                    , ssSpeed = fromMaybe [] <$> (fmap . fmap) snd ssVs
                    , gsSpeed = fromMaybe [] <$> (fmap . fmap) snd gsVs 
                    , nigh = dsNigh
                    , land = dsLand
                    }

includeTask :: [IxTask] -> IxTask -> Bool
includeTask tasks = if null tasks then const True else (`elem` tasks)


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
            $ (\(p, FlightStats{..}) ->
                ((p,) . positionAtEss) <$> statTimeRank)
            <$> xs

        pilots :: PilotsAtEss
        pilots = PilotsAtEss . toInteger $ length ys

        f position =
            TrackArrival
                { rank = position
                , frac = arrivalFraction pilots position
                }

times
    :: (TimeStats -> PilotTime (Quantity Double [u| h |]))
    -> [(Pilot, FlightStats)]
    -> Maybe (BestTime (Quantity Double [u| h |]), [(Pilot, TrackSpeed)])
times f xs =
    (\ bt -> (bt, sortOn (time . snd) $ second (g bt) <$> ys))
    <$> Gap.bestTime' ts
    where
        ys :: [(Pilot, PilotTime (Quantity Double [u| h |]))]
        ys =
            catMaybes
            $ (\(p, FlightStats{..}) -> ((p,) . f) <$> statTimeRank)
            <$> xs

        ts :: [PilotTime (Quantity Double [u| h |])]
        ts = snd <$> ys

        g best t =
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
flown math (RouteLookup lookupTaskLength) flying tags tasks iTask fixes =
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
flown' dTaskF flying math tags tasks iTask@(IxTask i) mf@MarkedFixes{mark0} p =
    case maybeTask of
        Nothing -> nullStats

        Just task' ->
            case (ssTime, gsTime, arrivalRank) of
                (Just a, Just b, Just c) ->
                    tickedStats {statTimeRank = Just $ TimeStats a b c}

                _ ->
                    tickedStats {statLand = Just $ landDistance task' }

    where
        maybeTask = tasks ^? element (i - 1)

        ticked = Lookup.ticked (tagTicked tags) mf iTask speedSection' p
        ssTime = Lookup.pilotTime (tagPilotTime tags) mf iTask [] speedSection' p
        gsTime = Lookup.pilotTime (tagPilotTime tags) mf iTask startGates' speedSection' p
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
                    { togo = unTaskDistance <$> togoAtLanding math ticked task xs
                    , made =
                        fromRational . unPilotDistance
                        <$> madeAtLanding math dTaskF ticked task xs
                    }

        startGates' =
            case tasks ^? element (fromIntegral i - 1) of
                Nothing -> []
                Just Task{..} -> startGates

        speedSection' =
            case tasks ^? element (fromIntegral i - 1) of
                Nothing -> Nothing
                Just Task{..} -> speedSection

