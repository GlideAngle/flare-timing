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
{-# LANGUAGE ParallelListComp #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.Maybe (catMaybes, isNothing)
import Data.List (nub, sort)
import Control.Lens ((^?), element)
import Control.Monad (mapM_)
import Control.Monad.Except (ExceptT(..), runExceptT)
import System.FilePath (takeFileName)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (CmdOptions(..), ProgramName(..), mkOptions)
import Flight.Comp
    ( FileType(CompInput)
    , CompInputFile(..)
    , CompSettings(..)
    , Pilot(..)
    , TrackFileFail(..)
    , IxTask(..)
    , compToCross
    , findCompInput
    , ensureExt
    )
import Flight.Units ()
import Flight.Track.Cross
    (TrackFlyingSection(..)
    , TrackCross(..)
    , PilotTrackCross(..)
    , Crossing(..)
    , trackLogErrors
    )
import Flight.LatLng.Rational (defEps)
import Flight.Zone.Raw (RawZone)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat (distanceHaversine)
import qualified Flight.Earth.Sphere.PointToPoint.Double as Dbl (distanceHaversine)
import Flight.Mask
    ( TaskZone
    , FnIxTask
    , FnTask
    , MadeZones(..)
    , SelectedCrossings(..)
    , NomineeCrossings(..)
    , unSelectedCrossings
    , unNomineeCrossings
    , checkTracks
    , madeZones
    , zoneToCylinder
    , nullFlying
    )
import Flight.Scribe (writeCrossing)
import CrossZoneOptions (description)
import Flight.Span.Math (Math(..))

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = ensureExt CompInput}
    err <- checkPaths lf options

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
            let errs = catMaybes . snd <$> ys

            let pErrs :: [[Pilot]] = (fmap. fmap) fst errs 

            let flying = (fmap . fmap . fmap . fmap) madeZonesToFlying ps

            let notFlys :: [[Pilot]] =
                    [ fmap fst . filter snd
                      $ (fmap . fmap) (maybe False (not . flew)) fs
                    | fs <- flying
                    ]

            let dnfs =
                    [ sort . nub $ es ++ ns
                    | es <- pErrs
                    | ns <- notFlys
                    ]

            let crossZone =
                    Crossing
                        { trackLogError = trackLogErrors <$> errs
                        , dnf = dnfs
                        , flying = flying
                        , crossing = (fmap . fmap) crossings ps
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

flew :: TrackFlyingSection -> Bool
flew TrackFlyingSection{flyingFixes, flyingSeconds}
    | isNothing flyingFixes = False
    | isNothing flyingSeconds = False
    | otherwise = f flyingFixes || f flyingSeconds
    where
        f :: Ord a => Maybe (a, a) -> Bool
        f Nothing = False
        f (Just (a, b)) = a < b

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

flown :: Math -> FnIxTask MadeZones
flown math tasks (IxTask i) fs =
    case tasks ^? element (i - 1) of
        Nothing ->
            MadeZones
                { flying = nullFlying
                , selectedCrossings = SelectedCrossings []
                , nomineeCrossings = NomineeCrossings []
                }

        Just task ->
            flownTask math task fs

flownTask :: Math -> FnTask MadeZones
flownTask =
    \case
        Rational ->
            madeZones
                (Rat.distanceHaversine defEps)
                (zoneToCylinder :: RawZone -> TaskZone Rational)
        Floating ->
            madeZones
                Dbl.distanceHaversine
                (zoneToCylinder :: RawZone -> TaskZone Double)
