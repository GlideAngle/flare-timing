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

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Driver (driverMain) where

import Data.Ratio ((%))
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import qualified Formatting as Fmt ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import qualified Data.Map.Strict as Map
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
import Flight.Track.Lead (TrackLead(..))
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Mask (Masking(..))
import Flight.Track.Point (Pointing(..), Allocation(..))
import qualified Flight.Track.Land as Cmp (Landing(..))
import Flight.Scribe
    (readComp, readCrossing, readMasking, readLanding, writePointing)
import Flight.Score
    ( MinimumDistance(..), MaximumDistance(..)
    , BestDistance(..), SumOfDistance(..)
    , PilotsAtEss(..), PilotsPresent(..), PilotsFlying(..)
    , GoalRatio(..), Lw(..), Aw(..)
    , NominalTime(..), BestTime(..)
    , Validity(..), ValidityWorking(..)
    , LeadingFraction(..), ArrivalFraction(..)
    , DistancePoints(..), LeadingPoints(..), ArrivalPoints(..), TimePoints(..)
    , TaskPoints(..)
    , distanceWeight, leadingWeight, arrivalWeight, timeWeight
    , taskValidity, launchValidity, distanceValidity, timeValidity
    , availablePoints
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
        }
        _ =
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

        leadingPoints :: [[(Pilot, LeadingPoints)]] =
            [ maybe
                []
                (\ps' -> (fmap . fmap) (applyLeading ps') ls)
                ps
            | ps <- (fmap . fmap) points allocs
            | ls <- lead
            ]

        arrivalPoints :: [[(Pilot, ArrivalPoints)]] =
            [ maybe
                []
                (\ps' -> (fmap . fmap) (applyArrival ps') ls)
                ps
            | ps <- (fmap . fmap) points allocs
            | ls <- arrival
            ]

        score :: [[(Pilot, (Gap.Points, TaskPoints))]] =
            [ ((fmap . fmap) tally) $ collate ls as
            | ls <- leadingPoints
            | as <- arrivalPoints
            ]

zeroPoints :: Gap.Points
zeroPoints =
    Gap.Points 
        { distance = DistancePoints 0
        , leading = LeadingPoints 0
        , arrival = ArrivalPoints 0
        , time = TimePoints 0
        }

applyLeading :: Gap.Points -> TrackLead -> LeadingPoints
applyLeading
    Gap.Points{leading = LeadingPoints y}
    TrackLead{frac = LeadingFraction x} =
    LeadingPoints $ x * y

applyArrival :: Gap.Points -> TrackArrival -> ArrivalPoints
applyArrival
    Gap.Points{arrival = ArrivalPoints y}
    TrackArrival{frac = ArrivalFraction x} =
    ArrivalPoints $ x * y

collate
    :: [(Pilot, LeadingPoints)]
    -> [(Pilot, ArrivalPoints)]
    -> [(Pilot, Gap.Points)]
collate ls as =
    Map.toList $ Map.intersectionWith glue ml ma
    where
        ml = Map.fromList ls
        ma = Map.fromList as

glue :: LeadingPoints -> ArrivalPoints -> Gap.Points
glue l a =
    zeroPoints {Gap.leading = l, Gap.arrival = a}

tally :: Gap.Points -> (Gap.Points, TaskPoints)
tally
    x@Gap.Points
        { distance = DistancePoints d
        , leading = LeadingPoints l
        , arrival = ArrivalPoints a
        , time = TimePoints t
        } =
    (x, TaskPoints $ d + l + a + t)

