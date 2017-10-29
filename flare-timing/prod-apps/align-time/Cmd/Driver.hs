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
import Data.Maybe (catMaybes)
import Control.Monad (mapM_)
import Control.Monad.Except (runExceptT)
import Data.UnitsOfMeasure (u, convert)
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
import Flight.Mask (SigMasking, checkTracks, groupByLeg, distancesToGoal)
import Flight.Track.Cross (Fix(..))
import Flight.Track.Time (TimeRow(..))
import Flight.Task (TaskDistance(..))
import Flight.Kml (MarkedFixes(..))
import Data.Number.RoundingFunctions (dpRound)

type Leg = Int

unTaskDistance :: Fractional a => TaskDistance -> a
unTaskDistance (TaskDistance d) =
    fromRational $ dpRound 3 dKm
    where 
        MkQuantity dKm = convert d :: Quantity Rational [u| km |]

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

                            _ <- sequence $ zipWith
                                (\ iTask rows ->
                                    if includeTask iTask then
                                        writeTimeRowsToCsv (fcsv iTask) headers rows
                                    else 
                                        return ())
                                [1 .. ]
                                (concat <$> ys)

                            return ()

                checkAll =
                    checkTracks $ \CompSettings{tasks} -> group tasks

                includeTask :: Int -> Bool
                includeTask = if null task then const True else (flip elem $ task)

                fcsv :: Int -> FilePath
                fcsv n =
                    flip replaceExtension ("." ++ show n ++ ".align-time.csv")
                    $ dropExtension yamlCompPath

mkTimeRows :: Leg
           -> Maybe [(Maybe Fix, Maybe TaskDistance)]
           -> Pilot
           -> [TimeRow]
mkTimeRows _ Nothing _ = []
mkTimeRows leg (Just xs) p = catMaybes $ mkTimeRow leg p <$> xs

mkTimeRow :: Int
          -> Pilot
          -> (Maybe Fix, Maybe TaskDistance)
          -> Maybe TimeRow
mkTimeRow _ _ (Nothing, _) = Nothing
mkTimeRow _ _ (_, Nothing) = Nothing
mkTimeRow leg p (Just Fix{time, lat, lng}, Just d) =
    Just $ TimeRow
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
            (\leg xs -> mkTimeRows leg (distancesToGoal tasks iTask xs) pilot)
            [1 .. ]
            ys
    where
        ys :: [MarkedFixes]
        ys = groupByLeg tasks iTask fs
