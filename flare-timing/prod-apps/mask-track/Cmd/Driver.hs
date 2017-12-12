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

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (sortOn)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Data.String (IsString)
import Control.Lens ((^?), element)
import Control.Monad (join, mapM_)
import Control.Applicative.Alternative ((<|>))
import Control.Monad.Except (ExceptT, runExceptT)
import Data.UnitsOfMeasure ((/:), u, convert, toRational', fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath.Find (FileType(..), (==?), (&&?), find, always, fileType, extension)
import System.FilePath (takeFileName, replaceExtension, dropExtension)
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString as BS
import qualified Data.Number.FixedFunctions as F
import Data.Aeson.ViaScientific (ViaScientific(..))

import Flight.Comp
    ( Pilot(..)
    , CompFile(..)
    , TagFile(..)
    , CompSettings(..)
    , Task(..)
    , TrackFileFail(..)
    , compToCross
    , crossToTag
    )
import qualified Flight.Task as Tsk (TaskDistance(..))
import qualified Flight.Score as Gap (PilotDistance(..), PilotTime(..))
import Flight.TrackLog (IxTask(..))
import Flight.Units ()
import Flight.Mask
    ( SigMasking
    , TaskZone
    , Ticked(..)
    , checkTracks
    , madeGoal
    , distanceToGoal
    , distanceFlown
    , timeFlown
    , zoneToCylinder
    )
import Flight.Track.Mask (Masking(..), PilotTrackMask(..), TrackArrival(..))
import qualified Flight.Track.Mask as TM (TrackMask(..))
import Flight.Track.Tag (Tagging)
import Flight.Zone (Bearing(..))
import Flight.Zone.Raw (RawZone)
import Flight.LatLng.Rational (Epsilon(..), defEps)
import Data.Number.RoundingFunctions (dpRound)
import Flight.Task (SpanLatLng, CircumSample, AngleCut(..))
import qualified Flight.PointToPoint.Rational as Rat
    (distanceHaversine, distancePointToPoint, costSegment)
import qualified Flight.PointToPoint.Double as Dbl
    (distanceHaversine, distancePointToPoint, costSegment)
import qualified Flight.Cylinder.Rational as Rat (circumSample)
import qualified Flight.Cylinder.Double as Dbl (circumSample)

import Flight.Cmd.Paths (checkPaths)
import Flight.Cmd.Options (Math(..), CmdOptions(..), ProgramName(..), mkOptions)
import Cmd.Options (description)
import Cmd.Inputs
    ( MadeGoal(..), ArrivalRank(..)
    , tagMadeGoal, tagArrivalRank
    , readTags
    )
import Flight.Score (PilotsAtEss(..), PositionAtEss(..), arrivalFraction)

driverMain :: IO ()
driverMain = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing
    err <- checkPaths options
    case err of
        Just msg -> putStrLn msg
        Nothing -> drive options

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        -- TODO: first start time & last goal time & launched
        ("rank", _) -> LT
        ("frac", _) -> GT
        ("madeGoal", _) -> LT
        ("arrivalRank", "madeGoal") -> GT
        ("arrivalRank", _) -> LT
        ("timeToGoal", "madeGoal") -> GT
        ("timeToGoal", "arrivalRank") -> GT
        ("timeToGoal", _) -> LT
        ("distanceToGoal", "madeGoal") -> GT
        ("distanceToGoal", "arrivalRank") -> GT
        ("distanceToGoal", "timeToGoal") -> GT
        ("distanceToGoal", _) -> LT
        ("distanceMade", _) -> GT
        _ -> compare a b

unTaskDistance :: (Real a, Fractional a) => Tsk.TaskDistance a -> a
unTaskDistance (Tsk.TaskDistance d) =
    fromRational $ dpRound 3 dKm
    where 
        MkQuantity dKm = toRational' $ convert d :: Quantity _ [u| km |]

unPilotDistance :: (Real a, Fractional b) => Gap.PilotDistance a -> b
unPilotDistance (Gap.PilotDistance d) =
    fromRational $ dpRound 3 dKm
    where 
        d' :: Quantity Rational [u| m |] = MkQuantity $ toRational d
        MkQuantity dKm = convert d' :: Quantity Rational [u| km |]

unPilotTime :: Fractional a => Gap.PilotTime -> a
unPilotTime (Gap.PilotTime t) = fromRational t

drive :: CmdOptions -> IO ()
drive CmdOptions{..} = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    dfe <- doesFileExist file
    if dfe then
        withFile (CompFile file)
    else do
        dde <- doesDirectoryExist dir
        if dde then do
            files <- find always (fileType ==? RegularFile &&? extension ==? ".comp-inputs.yaml") dir
            mapM_ withFile (CompFile <$> files)
        else
            putStrLn "Couldn't find any flight score competition yaml input files."
    end <- getTime Monotonic
    fprint ("Masking tracks completed in " % timeSpecs % "\n") start end
    where
        withFile compFile@(CompFile compPath) = do
            let tagFile@(TagFile tagPath) = crossToTag . compToCross $ compFile
            putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
            putStrLn $ "Reading zone tags from '" ++ takeFileName tagPath ++ "'"

            tags <- runExceptT $ readTags tagFile

            writeMask
                (IxTask <$> task)
                (Pilot <$> pilot)
                (CompFile compPath)
                (check math tags)

writeMask :: [IxTask]
          -> [Pilot]
          -> CompFile
          -> (CompFile
              -> [IxTask]
              -> [Pilot]
              -> ExceptT
                   String
                   IO
                   [
                       [Either
                           (Pilot, TrackFileFail)
                           (Pilot, Pilot -> (Maybe PositionAtEss, TM.TrackMask))
                       ]
                   ]
             )
          -> IO ()
writeMask selectTasks selectPilots compFile@(CompFile compPath) f = do
    checks <- runExceptT $ f compFile selectTasks selectPilots

    case checks of
        Left msg -> print msg
        Right comp -> do
            let ys :: [([(Pilot, Maybe PositionAtEss)], [PilotTrackMask])] =
                    unzip <$>
                    (fmap . fmap)
                        (\case
                            Left (p, _) ->
                                ((p, Nothing), PilotTrackMask p Nothing)

                            Right (p, g) ->
                                let (ar, tm) = g p
                                in ((p, ar), PilotTrackMask p (Just tm)))
                        comp

            let tzi = Masking { masking = snd <$> ys
                              , arrival = (arrivals . arriving) <$> (fst <$> ys)
                              }

            let yaml =
                    Y.encodePretty
                        (Y.setConfCompare cmp Y.defConfig)
                        tzi 

            BS.writeFile maskPath yaml
    where
        maskPath =
            flip replaceExtension ".mask-track.yaml"
            $ dropExtension compPath

arriving :: [(Pilot, Maybe PositionAtEss)] -> [(Pilot, PositionAtEss)]
arriving xs =
    catMaybes $ f <$> xs
    where
        f (_, Nothing) = Nothing
        f (p, Just e) = Just (p, e)

arrivals :: [(Pilot, PositionAtEss)] -> [(Pilot, TrackArrival)]
arrivals xs =
    sortOn (rank . snd)
    $ (fmap . fmap) (f (PilotsAtEss . toInteger $ length xs)) xs
    where
        f pilots position =
            TrackArrival
                { rank = position
                , frac = ViaScientific $ arrivalFraction pilots position
                }


check :: Math
      -> Either String Tagging
      -> CompFile
      -> [IxTask]
      -> [Pilot]
      -> ExceptT
          String
          IO
          [
              [Either
                  (Pilot, TrackFileFail)
                  (Pilot, Pilot -> (Maybe PositionAtEss, TM.TrackMask))
              ]
          ]
check math tags = checkTracks $ \CompSettings{tasks} ->
    flown math tags tasks

flown :: Math
      -> Either String Tagging
      -> SigMasking (Pilot -> (Maybe PositionAtEss, TM.TrackMask))
flown math tags tasks iTask@(IxTask i) xs p =
    (arrivalRank, trackMask)
    where
        arrivalRank =
            PositionAtEss . toInteger
            <$> join ((\f -> f p speedSection' iTask xs) <$> tArrivalRank)

        trackMask =
            TM.TrackMask
                { madeGoal = mg

                , distanceToGoal =
                    if mg then Nothing else unTaskDistance <$> dg math

                , distanceMade =
                    fromRational <$> if mg then Nothing else unPilotDistance <$> df math

                , timeToGoal =
                    if not mg then Nothing else unPilotTime <$> tf math
                }

        dppR = Rat.distancePointToPoint
        dppF = Dbl.distancePointToPoint

        csegR = Rat.costSegment spanR
        csegF = Dbl.costSegment spanF

        dg :: Math -> Maybe (Tsk.TaskDistance Double)
        dg =
            \case
            Floating ->
                distanceToGoal
                    noneTicked
                    spanF dppF csegF csF cutF
                    zoneToCylF tasks iTask xs

            Rational ->
                (\(Tsk.TaskDistance d) -> Tsk.TaskDistance $ fromRational' d) <$>
                distanceToGoal
                    noneTicked
                    spanR dppR csegR csR cutR
                    zoneToCylR tasks iTask xs

        df :: Math -> Maybe (Gap.PilotDistance Double)
        df =
            \case
            Floating ->
                distanceFlown
                    noneTicked
                    spanF dppF csegF csF cutF
                    zoneToCylF tasks iTask xs

            Rational ->
                (\(Gap.PilotDistance d) -> Gap.PilotDistance $ fromRational d) <$>
                distanceFlown
                    noneTicked
                    spanR dppR csegR csR cutR
                    zoneToCylR tasks iTask xs

        tf :: Math -> Maybe Gap.PilotTime
        tf =
            \case
            Floating ->
                timeFlown
                    spanF
                    zoneToCylF tasks iTask xs

            Rational ->
                timeFlown
                    spanR
                    zoneToCylR tasks iTask xs

        speedSection' =
            case tasks ^? element (fromIntegral i - 1) of
                Nothing -> Nothing
                Just Task{..} -> speedSection

        (MadeGoal tMadeGoal) = tagMadeGoal tags
        (ArrivalRank tArrivalRank) = tagArrivalRank tags

        mg :: Bool
        mg =
            let a = join $ (\f -> f p speedSection' iTask xs) <$> tMadeGoal
                b =
                    \case
                    Rational -> Just $ madeGoal spanR zoneToCylR tasks iTask xs
                    Floating -> Just $ madeGoal spanF zoneToCylF tasks iTask xs

            in fromMaybe False $ a <|> b math

zoneToCylR :: RawZone -> TaskZone Rational
zoneToCylR = zoneToCylinder

zoneToCylF :: RawZone -> TaskZone Double
zoneToCylF = zoneToCylinder

spanR :: SpanLatLng Rational
spanR = Rat.distanceHaversine defEps

spanF :: SpanLatLng Double
spanF = Dbl.distanceHaversine

csR :: CircumSample Rational
csR = Rat.circumSample

csF :: CircumSample Double
csF = Dbl.circumSample

cutR :: AngleCut Rational
cutR =
    AngleCut
        { sweep = let (Epsilon e) = defEps in Bearing . MkQuantity $ F.pi e
        , nextSweep = nextCutR
        }

cutF :: AngleCut Double
cutF =
    AngleCut
        { sweep = Bearing $ MkQuantity pi
        , nextSweep = nextCutF
        }

nextCutR :: AngleCut Rational -> AngleCut Rational
nextCutR x@AngleCut{sweep} =
    let (Bearing b) = sweep in x{sweep = Bearing $ b /: 2}

nextCutF :: AngleCut Double -> AngleCut Double
nextCutF x@AngleCut{sweep} =
    let (Bearing b) = sweep in x{sweep = Bearing $ b /: 2}

noneTicked :: Ticked
noneTicked = Ticked 0
