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
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cmd.Driver (driverMain) where

import Prelude hiding (span)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.Maybe (catMaybes)
import Control.Monad (mapM_, when, zipWithM)
import Control.Monad.Except (runExceptT)
import Data.UnitsOfMeasure ((/:), u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (FilePath, takeFileName, replaceExtension, dropExtension)
import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..))
import Cmd.Outputs (writeTimeRowsToCsv)

import Flight.Comp (CompSettings(..), Pilot(..))
import Flight.TrackLog (IxTask(..))
import Flight.Units ()
import Flight.Mask
    (TaskZone, SigMasking, checkTracks, groupByLeg, distancesToGoal, zoneToCylinder)
import Flight.Track.Cross (Fix(..))
import Flight.Zone (Bearing(..))
import Flight.Zone.Raw (RawZone)
import Flight.Track.Time (TimeRow(..))
import Flight.Kml (MarkedFixes(..))
import Data.Number.RoundingFunctions (dpRound)
import Flight.Task (TaskDistance(..), SpanLatLng, CircumSample, AngleCut(..))
import Flight.PointToPoint.Double
    (distanceHaversine, distancePointToPoint, costSegment)
import Flight.Cylinder.Double (circumSample)

type Leg = Int

unTaskDistance :: (Real a, Fractional a) => TaskDistance a -> a
unTaskDistance (TaskDistance d) =
    fromRational $ dpRound 3 dKm
    where 
        MkQuantity dKm = toRational' $ convert d :: Quantity _ [u| km |]

headers :: [String]
headers = ["leg", "time", "pilot", "lat", "lng", "distance"]

driverMain :: IO ()
driverMain = withCmdArgs drive

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
            putStrLn $ "Reading competition from '" ++ takeFileName yamlCompPath ++ "'"

            let go = writeTime yamlCompPath in go checkAll
            where
                writeTime yamlCompPath' f = do
                    checks <-
                        runExceptT $
                            f
                                yamlCompPath'
                                (IxTask <$> task)
                                (Pilot <$> pilot)

                    case checks of
                        Left msg -> print msg
                        Right xs -> do
                            let ys :: [[[TimeRow]]] =
                                    (fmap . fmap)
                                        (\case
                                            Left _ -> []
                                            Right (p, g) -> g p)
                                        xs

                            _ <- zipWithM
                                (\ iTask rows ->
                                    when (includeTask iTask) $
                                        writeTimeRowsToCsv (fcsv iTask) headers rows)
                                [1 .. ]
                                (concat <$> ys)

                            return ()

                checkAll =
                    checkTracks $ \CompSettings{tasks} -> group tasks

                includeTask :: Int -> Bool
                includeTask = if null task then const True else (`elem` task)

                fcsv :: Int -> FilePath
                fcsv n =
                    flip replaceExtension ("." ++ show n ++ ".align-time.csv")
                    $ dropExtension yamlCompPath

mkTimeRows :: Leg
           -> Maybe [(Maybe Fix, Maybe (TaskDistance Double))]
           -> Pilot
           -> [TimeRow]
mkTimeRows _ Nothing _ = []
mkTimeRows leg (Just xs) p = catMaybes $ mkTimeRow leg p <$> xs

mkTimeRow :: Int
          -> Pilot
          -> (Maybe Fix, Maybe (TaskDistance Double))
          -> Maybe TimeRow
mkTimeRow _ _ (Nothing, _) = Nothing
mkTimeRow _ _ (_, Nothing) = Nothing
mkTimeRow leg p (Just Fix{time, lat, lng}, Just d) =
    Just
        TimeRow
            { leg = leg
            , time = time
            , pilot = p
            , lat = lat
            , lng = lng
            , distance = unTaskDistance d
            }

group :: SigMasking (Pilot -> [TimeRow])
group tasks iTask fs =
    \pilot ->
        concat $ zipWith
            (\leg xs ->
                let xs' =
                        distancesToGoal
                            span dpp cseg cs cut
                            zoneToCyl tasks iTask xs
                in mkTimeRows leg xs' pilot)
            [1 .. ]
            ys
    where
        ys :: [MarkedFixes]
        ys = groupByLeg span zoneToCyl tasks iTask fs

        dpp = distancePointToPoint
        cseg = costSegment span

zoneToCyl :: RawZone -> TaskZone Double
zoneToCyl = zoneToCylinder

span :: SpanLatLng Double
span = distanceHaversine

cs :: CircumSample Double
cs = circumSample

cut :: AngleCut Double
cut =
    AngleCut
        { sweep = Bearing . MkQuantity $ pi
        , nextSweep = nextCut
        }

nextCut :: AngleCut Double -> AngleCut Double
nextCut x@AngleCut{sweep} =
    let (Bearing b) = sweep in x{sweep = Bearing $ b /: 2}
