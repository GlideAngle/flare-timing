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
import Data.List (transpose)
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
        { zonesFirst = firstTag <$> zs
        , zonesLast = lastTag <$> zs
        }
    where
        ys :: [[Maybe UTCTime]]
        ys = fromMaybe [] <$> tagTimes <$> xs

        zs = transpose ys

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
