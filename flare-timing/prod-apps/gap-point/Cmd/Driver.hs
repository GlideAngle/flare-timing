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

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Cmd.Driver (driverMain) where

import Data.Ratio ((%))
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import qualified Formatting as Fmt ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Monad.Except (runExceptT)
import System.FilePath (takeFileName)
import Flight.Cmd.Paths (checkPaths)
import Flight.Cmd.Options (CmdOptions(..), ProgramName(..), mkOptions)
import Cmd.Options (description)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Comp
    ( CompInputFile(..)
    , CompSettings(..)
    , Nominal(..)
    , CrossZoneFile(..)
    , MaskTrackFile(..)
    , LandOutFile(..)
    , compToCross
    , compToMask
    , compToLand
    , compToPoint
    , findCompInput
    )
import Flight.Units ()
import Flight.Track.Cross (Crossing(..))
import Flight.Track.Mask (Masking(..))
import Flight.Track.Point
    (Pointing(..), Validity(..), Allocation(..), Weight(..))
import qualified Flight.Track.Land as Cmp (Landing(..))
import Flight.Scribe
    (readComp, readCrossing, readMasking, readLanding, writePointing)
import Flight.Score
    ( NominalLaunch(..), NominalDistance(..), NominalTime(..)
    , MinimumDistance(..), MaximumDistance(..), SumOfDistance(..)
    , PilotsAtEss(..), GoalRatio(..), Lw(..), Aw(..)
    , Metres, BestTime(..)
    , distanceWeight, leadingWeight, arrivalWeight, timeWeight
    , launchValidity, distanceValidity, timeValidity
    )
import Data.Aeson.Via.Scientific (ViaSci(..))

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
            writePointing pointFile $ points cs cg mk lg

points :: CompSettings -> Crossing -> Masking -> Cmp.Landing -> Pointing
points
    CompSettings
        { pilots
        , nominal =
            Nominal{goal = gNom, distance = dNom, time = tNom'}
        }
    Crossing{dnf}
    Masking{pilotsAtEss, bestDistance, sumDistance, bestTime} _ =
    Pointing 
        { validity = validities
        , allocation = allocs
        }
    where
        tNom = 3600 * (read tNom' :: Double)

        lvs =
            [ launchValidity (NominalLaunch 1) ((p - d) % p)
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
                (NominalDistance $ round dNom)
                (p - d)
                (MinimumDistance [u| 5 km |])
                b
                s
            | p <- toInteger . length <$> pilots
            | d <- toInteger . length <$> dnf
            | b <- dBests
            | s <- dSums
            ]

        tBests :: [Maybe Metres] =
            [ round . (* 3600) <$> b
            | b <-
                  (fmap . fmap)
                      (\(ViaSci (BestTime x)) -> x)
                      bestTime
            ]

        tvs =
            [ timeValidity
                (NominalTime $ round tNom)
                (NominalDistance $ round dNom)
                t
                d
            | t <- tBests
            | d <- (\(MaximumDistance (MkQuantity x)) -> round x) <$> dBests
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
            [ Weight dw lw aw (timeWeight dw lw aw)
            | dw <- dws
            | lw <- lws
            | aw <- aws
            ]

        validities =
            [ Validity lv dv tv
            | lv <- lvs
            | dv <- dvs
            | tv <- tvs
            ]

        allocs =
            [ Allocation gr w
            | gr <- grs
            | w <- ws
            ]
