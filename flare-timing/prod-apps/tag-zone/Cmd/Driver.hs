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
import System.Clock (getTime, Clock(Monotonic))
import Data.String (IsString)
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
    ( FlownTrackCrossing(..)
    , FlownTrackTag(..)
    , PilotFlownTrackCrossing(..)
    , PilotFlownTrackTag(..)
    , PilotCrossings(..)
    , PilotTags(..)
    )
import Cmd.Inputs (readCrossings)

driverMain :: IO ()
driverMain = withCmdArgs drive

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
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
            putStrLn $ takeFileName yamlCrossZonePath
            let yamlTagZonePath =
                    flip replaceExtension ".tag-zone.yaml"
                    $ dropExtension yamlCrossZonePath
            writeTags yamlTagZonePath yamlCrossZonePath

writeTags :: FilePath -> FilePath -> IO ()
writeTags tagPath crossPath = do
    cs <- runExceptT $ readCrossings crossPath

    case cs of
        Left s -> putStrLn s
        Right PilotCrossings{pilotCrossings} -> do
            let ps :: [[PilotFlownTrackTag]] =
                    (fmap . fmap)
                        (\case
                            PilotFlownTrackCrossing p Nothing ->
                                PilotFlownTrackTag p Nothing

                            PilotFlownTrackCrossing p (Just xs) ->
                                PilotFlownTrackTag p (Just $ flown xs))
                        pilotCrossings

            let tzi =
                    PilotTags { pilotTags = ps }

            let yaml =
                    Y.encodePretty
                        (Y.setConfCompare cmp Y.defConfig)
                        tzi 

            BS.writeFile tagPath yaml

flown :: FlownTrackCrossing -> FlownTrackTag
flown FlownTrackCrossing{zonesCrossing} =
    FlownTrackTag
        { zonesTag = tagZones zonesCrossing
        }
