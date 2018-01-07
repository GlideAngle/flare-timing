{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Cmd.Driver (driverMain) where

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import Data.Time.Clock (UTCTime)
import System.Clock (getTime, Clock(Monotonic))
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (transpose, sortOn)
import Control.Monad (mapM_)
import Control.Monad.Except (runExceptT)
import System.FilePath (takeFileName)

import Flight.Cmd.Paths (checkPaths)
import Flight.Cmd.Options
    (CmdOptions(..), ProgramName(..), Extension(..), mkOptions)
import Cmd.Options (description)

import Flight.Units ()
import Flight.Mask (tagZones)
import Flight.Comp
    ( Pilot(..)
    , CrossZoneFile(..)
    , crossToTag
    , findCrossZone
    )
import Flight.Track.Cross
    (Crossing(..), TrackCross(..), PilotTrackCross(..), Fix(..))
import Flight.Track.Tag
    (Tagging(..), TrackTime(..), TrackTag(..), PilotTrackTag(..))
import Flight.Scribe (readCrossing, writeTagging)

driverMain :: IO ()
driverMain = do
    name <- getProgName
    options <- cmdArgs $ mkOptions
                            (ProgramName name)
                            description
                            (Just $ Extension "*.cross-zone.yaml")

    err <- checkPaths options
    maybe (drive options) putStrLn err

drive :: CmdOptions -> IO ()
drive o = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findCrossZone o
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ go files
    end <- getTime Monotonic
    fprint ("Tagging zones completed in " % timeSpecs % "\n") start end

go :: CrossZoneFile -> IO ()
go crossFile@(CrossZoneFile crossPath) = do
    putStrLn $ "Reading zone crossings from '" ++ takeFileName crossPath ++ "'"
    cs <- runExceptT $ readCrossing crossFile

    case cs of
        Left s -> putStrLn s
        Right Crossing{crossing} -> do
            let pss :: [[PilotTrackTag]] =
                    (fmap . fmap)
                        (\case
                            PilotTrackCross p Nothing ->
                                PilotTrackTag p Nothing

                            PilotTrackCross p (Just xs) ->
                                PilotTrackTag p (Just $ flown xs))
                        crossing

            let tagZone =
                    Tagging { timing = timed <$> pss
                            , tagging = pss
                            }

            writeTagging (crossToTag crossFile) tagZone

timed :: [PilotTrackTag] -> TrackTime
timed xs =
    TrackTime
        { zonesSum = length <$> rankTime
        , zonesFirst = firstTag <$> zs'
        , zonesLast = lastTag <$> zs'
        , zonesRankTime = rankTime
        , zonesRankPilot = (fmap . fmap) fst rs'
        }
    where
        zs :: [[Maybe UTCTime]]
        zs = fromMaybe [] . tagTimes <$> xs

        zs' :: [[Maybe UTCTime]]
        zs' = transpose zs

        rs :: [[Maybe (Pilot, UTCTime)]]
        rs = transpose $ rankByTag xs

        rs' :: [[(Pilot, UTCTime)]]
        rs' = sortOnTag <$> rs

        rankTime = (fmap . fmap) snd rs'

-- | Rank the pilots tagging each zone in a single task.
rankByTag :: [PilotTrackTag]
          -- ^ The list of pilots flying the task and the zones they tagged.
          -> [[Maybe (Pilot, UTCTime)]]
          -- ^ For each zone in the task, the sorted list of tag ordered pairs of
          -- pilots and their tag times.
rankByTag xs =
    (fmap . fmap) g zss
    where
        -- A list of pilots and maybe their tagged zones.
        ys :: [(Pilot, Maybe [Maybe UTCTime])]
        ys = (\t@(PilotTrackTag p _) -> (p, tagTimes t)) <$> xs

        f :: (Pilot, Maybe [Maybe UTCTime]) -> Maybe [(Pilot, Maybe UTCTime)]
        f (p, ts) = do
            ts' <- ts
            return $ (,) p <$> ts'

        -- For each zone, an unsorted list of pilots.
        zss :: [[(Pilot, Maybe UTCTime)]]
        zss = catMaybes $ f <$> ys

        -- Associate the pilot with each zone.
        g :: (Pilot, Maybe UTCTime) -> Maybe (Pilot, UTCTime)
        g (p, t) = do
            t' <- t
            return (p, t')

sortOnTag :: forall a. [Maybe (a, UTCTime)] -> [(a, UTCTime)]
sortOnTag xs =
    sortOn snd $ catMaybes xs

firstTag :: [Maybe UTCTime] -> Maybe UTCTime
firstTag xs =
    if null ys then Nothing else Just $ minimum ys
    where
        ys = catMaybes xs

lastTag :: [Maybe UTCTime] -> Maybe UTCTime
lastTag xs =
    if null ys then Nothing else Just $ maximum ys
    where
        ys = catMaybes xs

-- | Gets the pilots zone tag times.
tagTimes :: PilotTrackTag -> Maybe [Maybe UTCTime]
tagTimes (PilotTrackTag _ Nothing) = Nothing
tagTimes (PilotTrackTag _ (Just xs)) =
    Just $ fmap time <$> zonesTag xs

flown :: TrackCross -> TrackTag
flown TrackCross{zonesCrossSelected} =
    TrackTag
        { zonesTag = tagZones zonesCrossSelected
        }
