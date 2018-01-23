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
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Driver (driverMain) where

import Data.Ratio ((%))
import Data.Maybe (fromMaybe)
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import qualified Formatting as Fmt ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Control.Monad (mapM_)
import Control.Monad.Except (runExceptT)
import System.FilePath (takeFileName)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Cmd.Paths (checkPaths)
import Flight.Cmd.Options (CmdOptions(..), ProgramName(..), mkOptions)
import Flight.Comp
    ( CompInputFile(..)
    , CompSettings(..)
    , Nominal(..)
    , CrossZoneFile(..)
    , MaskTrackFile(..)
    , LandOutFile(..)
    , Pilot
    , compToCross
    , compToMask
    , compToLand
    , compToPoint
    , findCompInput
    )
import Flight.Units ()
import Flight.Track.Cross (Crossing(..))
import Flight.Track.Distance (TrackDistance(..), Nigh)
import Flight.Track.Lead (TrackLead(..))
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Speed (TrackSpeed(..))
import Flight.Track.Mask (Masking(..))
import Flight.Track.Land (Landing(..))
import Flight.Track.Point (Breakdown(..), Pointing(..), Allocation(..))
import qualified Flight.Track.Land as Cmp (Landing(..))
import Flight.Scribe
    (readComp, readCrossing, readMasking, readLanding, writePointing)
import Flight.Score
    ( MinimumDistance(..), MaximumDistance(..)
    , BestDistance(..), SumOfDistance(..), PilotDistance(..)
    , PilotsAtEss(..), PilotsPresent(..), PilotsFlying(..)
    , GoalRatio(..), Lw(..), Aw(..)
    , NominalTime(..), BestTime(..)
    , Validity(..), ValidityWorking(..)
    , DifficultyFraction(..), LeadingFraction(..)
    , ArrivalFraction(..), SpeedFraction(..)
    , DistancePoints(..), LinearPoints(..), DifficultyPoints(..)
    , LeadingPoints(..), ArrivalPoints(..), TimePoints(..)
    , TaskPoints(..)
    , IxChunk(..), ChunkDifficulty(..)
    , distanceWeight, leadingWeight, arrivalWeight, timeWeight
    , taskValidity, launchValidity, distanceValidity, timeValidity
    , availablePoints
    , toIxChunk
    )
import qualified Flight.Score as Gap (Validity(..), Points(..), Weights(..))
import Options (description)

driverMain :: IO ()
driverMain = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing
    err <- checkPaths options
    maybe (drive options) putStrLn err

drive :: CmdOptions -> IO ()
drive o = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findCompInput o
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    Fmt.fprint ("Tallying points completed in " Fmt.% timeSpecs Fmt.% "\n") start end

go :: CmdOptions -> CompInputFile -> IO ()
go CmdOptions{..} compFile@(CompInputFile compPath) = do
    let crossFile@(CrossZoneFile crossPath) = compToCross compFile
    let maskFile@(MaskTrackFile maskPath) = compToMask compFile
    let landFile@(LandOutFile landPath) = compToLand compFile
    let pointFile = compToPoint compFile
    putStrLn $ "Reading pilots absent from task from '" ++ takeFileName compPath ++ "'"
    putStrLn $ "Reading pilots that did not fly from '" ++ takeFileName crossPath ++ "'"
    putStrLn $ "Reading masked tracks from '" ++ takeFileName maskPath ++ "'"
    putStrLn $ "Reading distance difficulty from '" ++ takeFileName landPath ++ "'"

    compSettings <- runExceptT $ readComp compFile
    crossing <- runExceptT $ readCrossing crossFile
    masking <- runExceptT $ readMasking maskFile
    landing <- runExceptT $ readLanding landFile

    case (compSettings, crossing, masking, landing) of
        (Left msg, _, _, _) -> putStrLn msg
        (_, Left msg, _, _) -> putStrLn msg
        (_, _, Left msg, _) -> putStrLn msg
        (_, _, _, Left msg) -> putStrLn msg
        (Right cs, Right cg, Right mk, Right lg) -> do
            writePointing pointFile $ points' cs cg mk lg

points' :: CompSettings -> Crossing -> Masking -> Cmp.Landing -> Pointing
points'
    CompSettings
        { pilots
        , nominal =
            Nominal
                { launch = lNom
                , goal = gNom
                , distance = dNom
                , time = tNom
                , free
                }
        }
    Crossing{dnf}
    Masking
        { pilotsAtEss
        , bestDistance
        , sumDistance
        , bestTime
        , lead
        , arrival
        , speed
        , nigh
        }
    Landing
        { difficulty = landoutDifficulty
        } =
    Pointing 
        { validityWorking = workings
        , validity = validities
        , allocation = allocs
        , score = score
        }
    where
        -- NOTE: If there is no best distance, then either the task wasn't run
        -- or it has not been scored yet.
        maybeTasks :: [a -> Maybe a]
        maybeTasks =
            [ if null ds then const Nothing else Just | ds <- bestDistance ]

        lvs =
            [ launchValidity
                lNom
                (PilotsPresent . fromInteger $ p)
                (PilotsFlying . fromInteger $ p - d)
            | p <- toInteger . length <$> pilots
            | d <- toInteger . length <$> dnf
            ]

        dBests :: [MaximumDistance (Quantity Double [u| km |])] =
            [ MaximumDistance . MkQuantity $ maybe 0 id b
            | b <- bestDistance
            ]

        dSums :: [SumOfDistance (Quantity Double [u| km |])] =
            [ SumOfDistance . MkQuantity $ maybe 0 id s
            | s <- sumDistance
            ]

        dvs =
            [ distanceValidity
                gNom
                dNom
                (PilotsFlying $ p - d)
                (MinimumDistance [u| 5 km |])
                b
                s
            | p <- toInteger . length <$> pilots
            | d <- toInteger . length <$> dnf
            | b <- dBests
            | s <- dSums
            ]

        workings :: [Maybe ValidityWorking] =
            [ do
                lv' <- lv
                dv' <- dv
                tv' <- tv
                return $ ValidityWorking lv' dv' tv'
            | lv <- snd <$> lvs
            | dv <- snd <$> dvs
            | tv <- snd <$> tvs
            ]

        tvs =
            [ timeValidity
                ((\(NominalTime x) ->
                    NominalTime (convert x :: Quantity _ [u| s |])) tNom)
                t
                dNom
                d
            | t <-
                (fmap . fmap)
                    (\(BestTime x) -> BestTime (convert x :: Quantity _ [u| s |]))
                    bestTime

            | d <-
                (\(MaximumDistance x) -> BestDistance x)
                <$> dBests
            ]

        grs =
            [ GoalRatio $ n % toInteger (p - d)
            | n <- (\(PilotsAtEss x) -> x) <$> pilotsAtEss
            | p <- length <$> pilots
            | d <- length <$> dnf
            ]

        dws = distanceWeight <$> grs
        lws = leadingWeight . LwHg <$> dws
        aws = arrivalWeight . AwHg <$> dws

        ws =
            [ Gap.Weights dw lw aw (timeWeight dw lw aw)
            | dw <- dws
            | lw <- lws
            | aw <- aws
            ]

        validities =
            [ maybeTask $ Validity (taskValidity lv dv tv) lv dv tv
            | lv <- fst <$> lvs
            | dv <- fst <$> dvs
            | tv <- fst <$> tvs
            | maybeTask <- maybeTasks
            ]

        allocs =
            [ ((uncurry (Allocation gr w)) . (flip availablePoints w)) <$> v
            | gr <- grs
            | w <- ws
            | v <- (fmap . fmap) Gap.task validities
            ]

        difficultyDistancePoints :: [[(Pilot, DifficultyPoints)]] =
            [ maybe
                []
                (\ps' ->
                    let ld' = mapOfDifficulty ld
                        xs' = (fmap . fmap) (madeDifficulty free ld') xs
                        ys' = (fmap . fmap) (const $ DifficultyFraction 1) ys
                    in
                        (fmap . fmap)
                        (applyDifficulty ps')
                        (xs' ++ ys')
                )
                ps
            | ps <- (fmap . fmap) points allocs
            | xs <- nigh
            | ys <- arrival
            | ld <- landoutDifficulty
            ]

        linearDistancePoints :: [[(Pilot, LinearPoints)]] =
            [ maybe
                []
                (\ps' ->
                    let xs' = (fmap . fmap) madeLinear xs
                        ys' = (fmap . fmap) (const bd) ys
                    in
                        (fmap . fmap)
                        (applyLinear bd ps')
                        (xs' ++ ys')
                )
                ps
            | bd <- bestDistance
            | ps <- (fmap . fmap) points allocs
            | xs <- nigh
            | ys <- arrival
            ]

        leadingPoints :: [[(Pilot, LeadingPoints)]] =
            [ maybe
                []
                (\ps' ->
                    let xs' = (fmap . fmap) (const $ LeadingFraction 0) xs
                        ys' = (fmap . fmap) leadingFraction ys
                    in
                        (fmap . fmap)
                        (applyLeading ps')
                        (xs' ++ ys')
                )
                ps
            | ps <- (fmap . fmap) points allocs
            | xs <- nigh
            | ys <- lead
            ]

        arrivalPoints :: [[(Pilot, ArrivalPoints)]] =
            [ maybe
                []
                (\ps' ->
                    let xs' = (fmap . fmap) (const $ ArrivalFraction 0) xs
                        ys' = (fmap . fmap) arrivalFraction ys
                    in
                        (fmap . fmap)
                        (applyArrival ps')
                        (xs' ++ ys')
                )
                ps
            | ps <- (fmap . fmap) points allocs
            | xs <- nigh
            | ys <- arrival
            ]

        timePoints :: [[(Pilot, TimePoints)]] =
            [ maybe
                []
                (\ps' ->
                    let xs' = (fmap . fmap) (const $ SpeedFraction 0) xs
                        ys' = (fmap . fmap) speedFraction ys
                    in
                        (fmap . fmap)
                        (applyTime ps')
                        (xs' ++ ys')
                )
                ps
            | ps <- (fmap . fmap) points allocs
            | xs <- nigh
            | ys <- speed
            ]

        score :: [[(Pilot, Breakdown)]] =
            [ sortOn (total . snd)
              $ ((fmap . fmap) tally)
              $ collate diffs linears ls as ts
            | diffs <- difficultyDistancePoints
            | linears <- linearDistancePoints
            | ls <- leadingPoints
            | as <- arrivalPoints
            | ts <- timePoints
            ]

zeroPoints :: Gap.Points
zeroPoints =
    Gap.Points 
        { reach = LinearPoints 0
        , effort = DifficultyPoints 0
        , distance = DistancePoints 0
        , leading = LeadingPoints 0
        , arrival = ArrivalPoints 0
        , time = TimePoints 0
        }

mapOfDifficulty :: Maybe [ChunkDifficulty] -> Map IxChunk DifficultyFraction
mapOfDifficulty Nothing = Map.fromList []
mapOfDifficulty (Just xs) =
    Map.fromList $ (\ChunkDifficulty{chunk, frac} -> (chunk, frac)) <$> xs

applyDifficulty
    :: Gap.Points
    -> DifficultyFraction
    -> DifficultyPoints
applyDifficulty Gap.Points{effort = DifficultyPoints y} (DifficultyFraction frac) =
    DifficultyPoints $ frac * y

madeDistance :: TrackDistance Nigh -> PilotDistance (Quantity Double [u| km |])
madeDistance TrackDistance{made = Nothing} = PilotDistance . MkQuantity $ 0
madeDistance TrackDistance{made = Just d} = PilotDistance . MkQuantity $ d

madeDifficulty
    :: MinimumDistance (Quantity Double [u| km |])
    -> Map IxChunk DifficultyFraction
    -> TrackDistance Nigh
    -> DifficultyFraction
madeDifficulty md mapIxToFrac td =
    fromMaybe (DifficultyFraction 0) $ Map.lookup ix mapIxToFrac
    where
        pd = madeDistance td
        ix = toIxChunk md pd

madeLinear :: TrackDistance Nigh -> Maybe Double
madeLinear TrackDistance{made} = made

-- TODO: If made < minimum distance, use minimum distance.
applyLinear
    :: Maybe Double -- ^ The best distance
    -> Gap.Points
    -> Maybe Double -- ^ The distance made
    -> LinearPoints
applyLinear Nothing _ _ = LinearPoints 0
applyLinear _ _ Nothing = LinearPoints 0
applyLinear
    (Just best)
    Gap.Points{reach = LinearPoints y}
    (Just made) =
        if | best <= 0 -> LinearPoints 0
           | otherwise -> LinearPoints $ frac * y
    where
        frac :: Rational
        frac = toRational made / toRational best

leadingFraction :: TrackLead -> LeadingFraction
leadingFraction TrackLead{frac} = frac

applyLeading :: Gap.Points -> LeadingFraction -> LeadingPoints
applyLeading Gap.Points{leading = LeadingPoints y} (LeadingFraction x) =
    LeadingPoints $ x * y

arrivalFraction :: TrackArrival -> ArrivalFraction
arrivalFraction TrackArrival{frac} = frac

applyArrival :: Gap.Points -> ArrivalFraction -> ArrivalPoints
applyArrival Gap.Points{arrival = ArrivalPoints y} (ArrivalFraction x) =
    ArrivalPoints $ x * y

speedFraction :: TrackSpeed -> SpeedFraction
speedFraction TrackSpeed{frac} = frac

applyTime :: Gap.Points -> SpeedFraction -> TimePoints
applyTime Gap.Points{time = TimePoints y} (SpeedFraction x) =
    TimePoints $ x * y

collate
    :: [(Pilot, DifficultyPoints)]
    -> [(Pilot, LinearPoints)]
    -> [(Pilot, LeadingPoints)]
    -> [(Pilot, ArrivalPoints)]
    -> [(Pilot, TimePoints)]
    -> [(Pilot, Gap.Points)]
collate diffs linears ls as ts =
    Map.toList
    $ Map.intersectionWith glueDiff mDiff
    $ Map.intersectionWith glueLinear mLinear
    $ Map.intersectionWith glueTime mt
    $ Map.intersectionWith glueLA ml ma
    where
        mDiff = Map.fromList diffs
        mLinear = Map.fromList linears
        ml = Map.fromList ls
        ma = Map.fromList as
        mt = Map.fromList ts

glueDiff :: DifficultyPoints -> Gap.Points -> Gap.Points
glueDiff
    effort@(DifficultyPoints diff)
    p@Gap.Points {Gap.reach = LinearPoints linear} =
    p
        { Gap.effort = effort
        , Gap.distance = DistancePoints $ diff + linear
        }

glueLinear :: LinearPoints -> Gap.Points -> Gap.Points
glueLinear r p = p {Gap.reach = r}

glueLA :: LeadingPoints -> ArrivalPoints -> Gap.Points
glueLA l a = zeroPoints {Gap.leading = l, Gap.arrival = a}

glueTime :: TimePoints -> Gap.Points -> Gap.Points
glueTime t p = p {Gap.time = t}

tally :: Gap.Points -> Breakdown
tally
    x@Gap.Points
        { reach = LinearPoints r
        , effort = DifficultyPoints e
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        } =
    Breakdown
        { breakdown = x
        , total = TaskPoints $ r + e + l + a + t
        }

