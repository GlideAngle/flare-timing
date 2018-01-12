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

import Flight.Comp
    ( CompInputFile(..)
    , CompSettings(..)
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
import Flight.Track.Point (Pointing(..))
import qualified Flight.Track.Land as Cmp (Landing(..))
import Flight.Scribe
    (readComp, readCrossing, readMasking, readLanding, writePointing)
import Flight.Score (PilotsAtEss(..), GoalRatio(..))

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
points CompSettings{pilots} Crossing{dnf} Masking{pilotsAtEss} _ =
    Pointing 
        { goalRatio = gr
        }
    where
        gr =
            [ GoalRatio $ n % toInteger (p - d)
            | n <- (\(PilotsAtEss x) -> x) <$> pilotsAtEss
            | p <- length <$> pilots
            | d <- length <$> dnf
            ]
