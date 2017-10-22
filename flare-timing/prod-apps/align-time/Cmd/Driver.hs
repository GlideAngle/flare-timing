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
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (FilePath, takeFileName, replaceExtension, dropExtension)
import Cmd.Args (withCmdArgs)
import Cmd.Options (CmdOptions(..))
import Cmd.Outputs (writeTimeRowsToCsv)

import Flight.Comp (CompSettings(..), Pilot(..))
import Flight.TrackLog (TrackFileFail(..), IxTask(..))
import Flight.Units ()
import Flight.Mask (SigMasking)
import Flight.Mask.Pilot (checkTracks, distancesToGoal)
import Flight.Track.Cross (Fix(..))
import Flight.Track.Time (TimeRow(..))
import Flight.Task (TaskDistance(..))
import Data.Number.RoundingFunctions (dpRound)

type MkPart a =
    FilePath
    -> [IxTask]
    -> [Pilot]
    -> ExceptT
        String
        IO
        [[Either
            (Pilot, TrackFileFail)
            (Pilot, a)
        ]]

type AddPart a = a -> (Pilot -> [TimeRow])

type MkDistanceTrackIO a =
    FilePath
    -> MkPart a
    -> AddPart a
    -> IO ()

unTaskDistance :: Fractional a => TaskDistance -> a
unTaskDistance (TaskDistance d) =
    fromRational $ dpRound 3 dKm
    where 
        MkQuantity dKm = convert d :: Quantity Rational [u| km |]

headers :: [String]
headers = ["time", "pilot", "lat", "lng", "distance"]

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

            let go = writeTime yamlCompPath in go checkAll mkTimeRows
            where
                writeTime :: forall a. MkDistanceTrackIO a
                writeTime yamlCompPath' f g = do
                    checks <-
                        runExceptT $
                            f
                                yamlCompPath'
                                (IxTask <$> task)
                                (Pilot <$> pilot)

                    case checks of
                        Left msg -> print msg
                        Right xs -> do
                            let ts :: [[[TimeRow]]] =
                                    (fmap . fmap)
                                        (\case
                                            Left _ -> []
                                            Right (p, d) -> g d $ p)
                                        xs

                            let ts' :: [[TimeRow]] = concat <$> ts

                            _ <- sequence $ zipWith
                                (\ iTask rows ->
                                    writeTimeRowsToCsv (fcsv iTask) headers rows)
                                [1 .. ]
                                ts'

                            return ()

                checkAll =
                    checkTracks $ \CompSettings{tasks} -> flown tasks

                flown :: SigMasking (Maybe [(Maybe Fix, Maybe TaskDistance)])
                flown tasks iTask xs =
                    distancesToGoal tasks iTask xs

                fcsv :: Int -> FilePath
                fcsv n =
                    flip replaceExtension ("." ++ show n ++ ".align-time.yaml")
                    $ dropExtension yamlCompPath

mkTimeRows :: Maybe [(Maybe Fix, Maybe TaskDistance)]
           -> Pilot
           -> [TimeRow]
mkTimeRows Nothing _ = []
mkTimeRows (Just xs) p = catMaybes $ mkTimeRow p <$> xs

mkTimeRow :: Pilot
          -> (Maybe Fix, Maybe TaskDistance)
          -> Maybe TimeRow
mkTimeRow _ (Nothing, _) = Nothing
mkTimeRow _ (_, Nothing) = Nothing
mkTimeRow p (Just Fix{time, lat, lng}, Just d) =
    Just $ TimeRow
        { time = time
        , pilot = p
        , lat = lat
        , lng = lng
        , distance = unTaskDistance d
        }
