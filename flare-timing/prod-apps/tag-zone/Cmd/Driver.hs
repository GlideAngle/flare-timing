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

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Cmd.Driver (driverMain) where

import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import Data.Time.Clock (UTCTime)
import System.Clock (getTime, Clock(Monotonic))
import Data.String (IsString)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (transpose, sortOn)
import Control.Monad (mapM_)
import Control.Monad.Except (runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (FilePath, takeFileName, replaceExtension, dropExtension)
import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..))
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS

import Flight.Units ()
import Flight.Mask.Pilot (tagZones)
import Data.Flight.Comp (Pilot(..))
import Data.Flight.PilotTrack
    ( TaggedTracks(..)
    , FlownTrackCrossing(..)
    , FlownTrackTag(..)
    , PilotFlownTrackCrossing(..)
    , PilotFlownTrackTag(..)
    , PilotCrossings(..)
    , TaskTiming(..)
    , Fix(..)
    )
import Cmd.Inputs (readCrossings)

driverMain :: IO ()
driverMain = withCmdArgs drive

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        ("timing", _) -> LT
        ("pilotTags", _) -> GT
        ("time", _) -> LT
        ("lat", "time") -> GT
        ("lat", _) -> LT
        ("lng", _) -> GT
        _ -> compare a b

drive :: CmdOptions -> IO ()
drive CmdOptions{..} = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    dfe <- doesFileExist file
    if dfe then
        withFile file
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- find always (fileType ==? RegularFile &&? extension ==? ".cross-zone.yaml") dir
            mapM_ withFile files
        else
            putStrLn "Couldn't find any '.cross-zone.yaml' input files."
    end <- getTime Monotonic
    fprint ("Tagging zones completed in " % timeSpecs % "\n") start end
    where
        withFile yamlCrossZonePath = do
            let yamlTagZonePath =
                    flip replaceExtension ".tag-zone.yaml"
                    $ dropExtension yamlCrossZonePath

            putStrLn $ "Reading zone crossings from '" ++ takeFileName yamlCrossZonePath ++ "'"
            writeTags yamlTagZonePath yamlCrossZonePath

writeTags :: FilePath -> FilePath -> IO ()
writeTags tagPath crossPath = do
    cs <- runExceptT $ readCrossings crossPath

    case cs of
        Left s -> putStrLn s
        Right PilotCrossings{pilotCrossings} -> do
            let pss :: [[PilotFlownTrackTag]] =
                    (fmap . fmap)
                        (\case
                            PilotFlownTrackCrossing p Nothing ->
                                PilotFlownTrackTag p Nothing

                            PilotFlownTrackCrossing p (Just xs) ->
                                PilotFlownTrackTag p (Just $ flown xs))
                        pilotCrossings

            let tzi =
                    TaggedTracks { timing = timed <$> pss
                                 , pilotTags = pss
                                 }

            let yaml =
                    Y.encodePretty
                        (Y.setConfCompare cmp Y.defConfig)
                        tzi 

            BS.writeFile tagPath yaml

timed :: [PilotFlownTrackTag] -> TaskTiming
timed xs =
    TaskTiming
        { zonesFirst = firstTag <$> zs'
        , zonesLast = lastTag <$> zs'
        , zonesRankTime = (fmap . fmap) snd rs'
        , zonesRankPilot = (fmap . fmap) fst rs'
        }
    where
        zs :: [[Maybe UTCTime]]
        zs = fromMaybe [] <$> tagTimes <$> xs

        zs' :: [[Maybe UTCTime]]
        zs' = transpose zs

        rs :: [[Maybe (Pilot, UTCTime)]]
        rs = transpose $ rankByTag xs

        rs' :: [[(Pilot, UTCTime)]]
        rs' = sortOnTag <$> rs

-- | Rank the pilots tagging each zone in a single task.
rankByTag :: [PilotFlownTrackTag]
          -- ^ The list of pilots flying the task and the zones they tagged.
          -> [[Maybe (Pilot, UTCTime)]]
          -- ^ For each zones in the task the sorted list of tag order pairs of
          -- pilots and their tag times.
rankByTag xs =
    (fmap . fmap) g zss
    where
        -- A list of pilots and maybe their tagged zones.
        ys :: [(Pilot, Maybe [Maybe UTCTime])]
        ys = (\t@(PilotFlownTrackTag p _) -> (p, tagTimes t)) <$> xs

        f :: (Pilot, Maybe [Maybe UTCTime]) -> Maybe [(Pilot, Maybe UTCTime)]
        f (p, ts) = do
            ts' <- ts
            return $ ((,) p) <$> ts'

        -- For each zone, an unsorted list of pilots.
        zss :: [[(Pilot, Maybe UTCTime)]]
        zss = catMaybes $ f <$> ys

        -- Associate the pilot with each zone.
        g :: (Pilot, Maybe UTCTime) -> Maybe (Pilot, UTCTime)
        g (p, t) = do
            t' <- t
            return $ (p, t')

sortOnTag :: forall a. [Maybe (a, UTCTime)] -> [(a, UTCTime)]
sortOnTag xs =
    (sortOn snd) $ catMaybes xs

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
tagTimes :: PilotFlownTrackTag -> Maybe [Maybe UTCTime]
tagTimes (PilotFlownTrackTag _ Nothing) = Nothing
tagTimes (PilotFlownTrackTag _ (Just xs)) =
    Just $ (fmap . fmap) time $ zonesTag xs

flown :: FlownTrackCrossing -> FlownTrackTag
flown FlownTrackCrossing{zonesCrossing} =
    FlownTrackTag
        { zonesTag = tagZones zonesCrossing
        }
