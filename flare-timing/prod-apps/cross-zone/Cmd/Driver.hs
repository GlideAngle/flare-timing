{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Cmd.Driver (driverMain) where

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.Maybe (catMaybes)
import Control.Monad (mapM_)
import Control.Monad.Except (ExceptT(..), runExceptT)
import System.FilePath (takeFileName)
import Flight.Cmd.Paths (checkPaths)
import Flight.Cmd.Options (Math(..), CmdOptions(..), ProgramName(..), mkOptions)
import Cmd.Options (description)

import Flight.Comp
    ( CompInputFile(..)
    , CompSettings(..)
    , Pilot(..)
    , TrackFileFail(..)
    , compToCross
    , findCompInput
    )
import Flight.TrackLog (IxTask(..))
import Flight.Units ()
import Flight.Track.Cross
    (TrackFlyingSection(..), TrackCross(..), PilotTrackCross(..), Crossing(..))
import Flight.Zone.Raw (RawZone)
import qualified Flight.PointToPoint.Rational as Rat (distanceHaversine)
import qualified Flight.PointToPoint.Double as Dbl (distanceHaversine)
import Flight.LatLng.Rational (defEps)
import Flight.Mask
    ( TaskZone, SigMasking, MadeZones(..)
    , unSelectedCrossings, unNomineeCrossings
    , checkTracks, madeZones, zoneToCylinder
    )
import Flight.Yaml (writeCrossing)

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
    fprint ("Tracks crossing zones completed in " % timeSpecs % "\n") start end

go :: CmdOptions -> CompInputFile -> IO ()
go CmdOptions{..} compFile@(CompInputFile compPath) = do
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    writeMask
        compFile
        (IxTask <$> task)
        (Pilot <$> pilot)
        (checkAll math)

writeMask :: CompInputFile
          -> [IxTask]
          -> [Pilot]
          -> (CompInputFile
              -> [IxTask]
              -> [Pilot]
              -> ExceptT
                      String
                      IO [[Either (Pilot, TrackFileFail) (Pilot, MadeZones)]])
          -> IO ()
writeMask compFile task pilot f = do
    checks <- runExceptT $ f compFile task pilot

    case checks of
        Left msg -> print msg
        Right xs -> do

            let ys :: [([(Pilot, Maybe MadeZones)], [Maybe (Pilot, TrackFileFail)])] =
                    unzip <$>
                    (fmap . fmap)
                        (\case
                            Left err@(p, _) ->
                                ((p, Nothing), Just err)

                            Right (p, x) ->
                                ((p, Just x), Nothing))
                        xs

            let ps = fst <$> ys

            let crossZone =
                    Crossing { crossing = (fmap . fmap) crossings ps
                             , errors = catMaybes . snd <$> ys
                             , flying = (fmap . fmap . fmap . fmap) madeZonesToFlying ps
                             }

            writeCrossing (compToCross compFile) crossZone

madeZonesToCross :: MadeZones -> TrackCross
madeZonesToCross x =
    TrackCross
        { zonesCrossSelected = unSelectedCrossings . selectedCrossings $ x
        , zonesCrossNominees = unNomineeCrossings . nomineeCrossings $ x
        }

crossings :: (Pilot, Maybe MadeZones) -> PilotTrackCross
crossings (p, x) =
    PilotTrackCross p $ madeZonesToCross <$> x

madeZonesToFlying :: MadeZones -> TrackFlyingSection
madeZonesToFlying MadeZones{flying} = flying

checkAll :: Math
         -> CompInputFile
         -> [IxTask]
         -> [Pilot]
         -> ExceptT
             String
             IO
             [[Either (Pilot, TrackFileFail) (Pilot, MadeZones)]]
checkAll math =
    checkTracks $ \CompSettings{tasks} -> flown math tasks

flown :: Math -> SigMasking MadeZones
flown =
    \case
        Rational ->
            madeZones
                (Rat.distanceHaversine defEps)
                (zoneToCylinder :: RawZone -> TaskZone Rational)
        Floating ->
            madeZones
                Dbl.distanceHaversine
                (zoneToCylinder :: RawZone -> TaskZone Double)
