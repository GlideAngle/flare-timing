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
import Control.Monad.Except (ExceptT(..), runExceptT)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (FilePath, takeFileName, replaceExtension, dropExtension)
import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..))
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS

import qualified Data.Flight.Comp as Cmp (CompSettings(..), Pilot(..))
import Data.Flight.TrackLog (TrackFileFail(..), IxTask(..))
import Flight.Units ()
import Flight.Mask (Masking)
import Flight.Mask.Pilot (checkTracks, madeZones)
import qualified Data.Flight.PilotTrack as TZ
    ( FlownTrackCrossing(..)
    , PilotFlownTrackCrossing(..)
    , PilotCrossings(..)
    )

type MkPart a =
    FilePath
    -> [IxTask]
    -> [Cmp.Pilot]
    -> ExceptT
        String
        IO
        [[Either
            (Cmp.Pilot, TrackFileFail)
            (Cmp.Pilot, a)
        ]]

type AddPart a = a -> TZ.FlownTrackCrossing

type MkCrossingTrackIO a =
    FilePath
    -> MkPart a
    -> AddPart a
    -> IO ()

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
            files <- find always (fileType ==? RegularFile &&? extension ==? ".comp-inputs.yaml") dir
            mapM_ withFile files
        else
            putStrLn "Couldn't find any flight score competition yaml input files."
    end <- getTime Monotonic
    fprint ("Tracks crossing zones completed in " % timeSpecs % "\n") start end
    where
        withFile yamlCompPath = do
            putStrLn $ takeFileName yamlCompPath
            let yamlMaskPath =
                    flip replaceExtension ".cross-zone.yaml"
                    $ dropExtension yamlCompPath
            let go = writeMask yamlMaskPath in go checkAll id
            where
                writeMask :: forall a. MkCrossingTrackIO a
                writeMask yamlPath f g = do
                    checks <-
                        runExceptT $
                            f
                                yamlCompPath
                                (IxTask <$> task)
                                (Cmp.Pilot <$> pilot)

                    case checks of
                        Left msg -> print msg
                        Right xs -> do
                            let ps :: [[TZ.PilotFlownTrackCrossing]] =
                                    (fmap . fmap)
                                        (\case
                                            Left (p, _) ->
                                                TZ.PilotFlownTrackCrossing p Nothing

                                            Right (p, x) ->
                                                TZ.PilotFlownTrackCrossing p (Just $ g x))
                                        xs

                            let tzi =
                                    TZ.PilotCrossings { pilotCrossings = ps }

                            let yaml =
                                    Y.encodePretty
                                        (Y.setConfCompare cmp Y.defConfig)
                                        tzi 

                            BS.writeFile yamlPath yaml

                checkAll =
                    checkTracks $ \Cmp.CompSettings{tasks} -> flown tasks

                flown :: Masking TZ.FlownTrackCrossing
                flown tasks iTask xs =
                    let (_, zp) = madeZones tasks iTask xs
                    in TZ.FlownTrackCrossing
                        { zonesCrossing = zp
                        }
