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
import Data.Time.Clock (diffUTCTime)
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
import Flight.Mask
    ( Sliver(..), FnIxTask, TaskZone, RaceSections(..)
    , checkTracks, dashDistanceToGoal, dashDistanceFlown, zoneToCylinder
    )
import Flight.Track.Mask
    ( Masking(..)
    , TrackArrival(..)
    , TrackSpeed(..)
    , TrackBestDistance(..)
    , TrackDistance(..)
    )
import Flight.Track.Tag (Tagging)
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
import Flight.Lookup.Tag
    ( ArrivalRankLookup(..), TimeLookup(..), TickLookup(..), StartEnd
    , tagArrivalRank, tagPilotTime, tagTicked
    )
import Flight.Scribe
    (readTagging, readRoute, writeMasking, readDiscardFurther)
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
        , statDistance :: Maybe TrackBestDistance
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
    let tagFile@(TagZoneFile tagPath) = crossToTag . compToCross $ compFile
    let lenFile@(TaskLengthFile lenPath) = compToTaskLength $ compFile
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading task length from '" ++ takeFileName lenPath ++ "'"
    putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"

    tags <- runExceptT $ readTagging tagFile
    routes <- runExceptT $ readRoute lenFile
    let lookupTaskLength = routeLength routes

    writeMask
        lookupTaskLength
        (IxTask <$> task)
        (Pilot <$> pilot)
        (CompInputFile compPath)
        (check math lookupTaskLength tags)

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
                            Left (p, _) -> (p, FlightStats Nothing Nothing)
                            Right (p, g) -> (p, g p))
                        comp

            let iTasks = IxTask <$> [1 .. length ys]
            let ds' :: [[(Pilot, TrackBestDistance)]] = distances <$> ys
            let as :: [[(Pilot, TrackArrival)]] = arrivals <$> ys
            let vs :: [Maybe (BestTime, [(Pilot, TrackSpeed)])] = times <$> ys

            bs'' :: [[Maybe (Pilot, Time.TickRow)]]
                    <- readCompBestDistances
                        compFile selectTasks ((fmap . fmap) fst ds')

            let bs' :: [[(Pilot, Time.TickRow)]] = catMaybes <$> bs''
            let bs :: [Map Pilot Time.TickRow] = Map.fromList <$> bs'
            let bestTime = (fmap . fmap) (ViaScientific . fst) vs

            let ls :: [Maybe (TaskDistance Double)] =
                    (\i -> join ((\g -> g i) <$> lookupTaskLength))
                    <$> iTasks

            let ds = zipWith3 mergeTaskBestDistance bs ls ds'

            let us'' :: [[Maybe Double]] =
                    (fmap . fmap) (join . fmap made . best . snd) ds

            let us' :: [[Double]] = catMaybes <$> us''

            let us :: [Maybe Double] =
                    (\xs -> if null xs then Nothing else Just . maximum $ xs)
                    <$> us'

            let bestDistance :: [Maybe Double] =
                    zipWith3
                        (\td bt bd ->
                            if isJust bt then unTaskDistance <$> td
                                         else bd)
                        ls
                        bestTime
                        us

            let maskTrack =
                    Masking
                        { pilotsAtEss =
                            (PilotsAtEss . toInteger . length) <$> as

                        , bestTime = bestTime
                        , taskDistance = (fmap . fmap) unTaskDistance ls
                        , bestDistance = bestDistance
                        , arrival = as
                        , speed = (fromMaybe []) <$> (fmap . fmap) snd vs
                        , distance = ds
                        }

            writeMasking (compToMask compFile) maskTrack

mergeTaskBestDistance
    :: Map Pilot Time.TickRow
    -> Maybe (TaskDistance Double)
    -> [(Pilot, TrackBestDistance)]
    -> [(Pilot, TrackBestDistance)]
mergeTaskBestDistance m td =
    sortOn (fmap togo . best . snd)
    . fmap (mergePilotBestDistance m td)

mergePilotBestDistance
    :: Map Pilot Time.TickRow
    -> Maybe (TaskDistance Double)
    -> (Pilot, TrackBestDistance)
    -> (Pilot, TrackBestDistance)
mergePilotBestDistance m td pd@(p, d) =
    maybe
        pd
        ((p,) . madeDistance td d)
        (Map.lookup p m)

madeDistance
    :: Maybe (TaskDistance Double)
    -> TrackBestDistance
    -> Time.TickRow
    -> TrackBestDistance

madeDistance Nothing d Time.TickRow{distance} =
    d{ best =
        Just
        $ TrackDistance
            { togo = Just distance
            , made = Nothing
            }
     }

madeDistance (Just (TaskDistance td)) d Time.TickRow{distance} =
    d{ best =
        Just
        $ TrackDistance
            { togo = Just distance
            , made = Just . unTaskDistance . TaskDistance $ td -: togo'
            }
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

distances :: [(Pilot, FlightStats)] -> [(Pilot, TrackBestDistance)]
distances xs =
    catMaybes
    $ fmap (\(p, FlightStats{..}) -> (p,) <$> statDistance) xs

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
check math lengths tags = checkTracks $ \CompSettings{tasks} ->
    flown math lengths tags tasks

flown :: Math
      -> RouteLookup
      -> Either String Tagging
      -> FnIxTask (Pilot -> FlightStats)
flown math (RouteLookup lookupTaskLength) tags tasks iTask xs =
    maybe
        (const $ FlightStats Nothing Nothing)
        (\d -> flown' d math tags tasks iTask xs)
        taskLength
    where
        taskLength = join ((\f -> f iTask) <$> lookupTaskLength)

flown' :: TaskDistance Double
       -> Math
       -> Either String Tagging
       -> FnIxTask (Pilot -> FlightStats)
flown' dTaskF@(TaskDistance td) math tags tasks iTask@(IxTask i) xs p =
    case tasks ^? element (i - 1) of
        Nothing -> FlightStats Nothing Nothing
        Just task' ->
            case (pilotTime, arrivalRank) of
                (Nothing, _) -> FlightStats Nothing (Just $ distance task')
                (_, Nothing) -> FlightStats Nothing (Just $ distance task')
                (Just a, Just b) -> FlightStats (Just (a, b)) Nothing
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
            TrackBestDistance
                { best = Nothing
                , last =
                    Just
                    $ TrackDistance
                        { togo = unTaskDistance <$> dgLast task math
                        , made = fromRational <$> unPilotDistance <$> dfLast task math
                        }
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
