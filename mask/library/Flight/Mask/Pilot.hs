{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Data.Flight.Mask
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Mask tracks with zones, working out; did the pilot launch, did they make goaland how
long did that take? If they didn't make goal then what zones did they make and what
was the distance to goal?
-}
module Flight.Mask.Pilot
    ( countFixes
    , checkTracks
    , madeZones
    , madeSpeedZones
    , launched
    , madeGoal
    , started
    , distanceToGoal
    , distanceFlown
    , timeFlown
    ) where

import Data.Time.Clock (UTCTime, diffUTCTime, addUTCTime)
import Data.List (nub)
import qualified Data.List as List (find, findIndex)
import Data.Ratio ((%))
import Data.UnitsOfMeasure ((-:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..), unQuantity)
import Control.Lens ((^?), element)
import Control.Monad.Except (ExceptT(..), lift)
import System.FilePath (FilePath, takeDirectory)

import qualified Data.Flight.Kml as Kml
    ( Fix
    , Seconds(..)
    , Latitude(..)
    , Longitude(..)
    , LatLngAlt(..)
    , FixMark(..)
    , MarkedFixes(..)
    )
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng.Raw (RawLat(..), RawLng(..))
import Flight.Zone (Radius(..), Zone(..))
import qualified Flight.Zone.Raw as Raw (RawZone(..))
import Data.Flight.PilotTrack (ZoneProof(..))
import qualified Data.Flight.PilotTrack as Cmp (Fix(..))
import qualified Data.Flight.Comp as Cmp
    ( CompSettings(..)
    , Pilot(..)
    , Task(..)
    , PilotTrackLogFile(..)
    , SpeedSection
    , OpenClose(..)
    , StartGate(..)
    )
import Data.Flight.TrackLog as Log
    ( TrackFileFail(..)
    , IxTask(..)
    , pilotTracks
    , filterPilots
    , filterTasks
    , makeAbsolute
    )
import Flight.Task as Tsk
    ( TaskDistance(..)
    , EdgeDistance(..)
    , Tolerance(..)
    , DistancePath(..)
    , separatedZones
    , distanceEdgeToEdge
    )
import Flight.Score as Gap (PilotDistance(..), PilotTime(..))
import Flight.Units ()
import Flight.Mask.Settings (readCompSettings)
import Flight.Mask (Masking)

newtype PilotTrackFixes = PilotTrackFixes Int deriving Show

type ZoneIdx = Int

data ZoneHit
    = ZoneMiss
    | ZoneEnter ZoneIdx ZoneIdx
    | ZoneExit ZoneIdx ZoneIdx
    deriving Eq

settingsLogs :: FilePath
             -> [IxTask]
             -> [Cmp.Pilot]
             -> ExceptT String IO (Cmp.CompSettings, [[Cmp.PilotTrackLogFile]])
settingsLogs compYamlPath tasks selectPilots = do
    settings <- readCompSettings compYamlPath
    ExceptT . return $ go settings
    where
        go s@Cmp.CompSettings{pilots, taskFolders} =
            Right (s, zs)
            where
                dir = takeDirectory compYamlPath
                ys = Log.filterPilots selectPilots $ Log.filterTasks tasks pilots
                fs = Log.makeAbsolute dir <$> taskFolders
                zs = zipWith (<$>) fs ys

checkTracks :: forall a. (Cmp.CompSettings -> (IxTask -> Kml.MarkedFixes -> a))
            -> FilePath
            -> [IxTask]
            -> [Cmp.Pilot]
            -> ExceptT
                String
                IO
                [[ Either
                   (Cmp.Pilot, TrackFileFail)
                   (Cmp.Pilot, a)
                ]]
checkTracks f compYamlPath tasks selectPilots = do
    (settings, xs) <- settingsLogs compYamlPath tasks selectPilots
    lift $ Log.pilotTracks (f settings) xs

countFixes :: Kml.MarkedFixes -> PilotTrackFixes
countFixes Kml.MarkedFixes{fixes} =
    PilotTrackFixes $ length fixes

-- | The input pair is in degrees while the output is in radians.
toLL :: (Rational, Rational) -> LatLng [u| rad |]
toLL (lat, lng) =
    LatLng (Lat lat'', Lng lng'')
        where
            lat' = MkQuantity lat :: Quantity Rational [u| deg |]
            lng' = MkQuantity lng :: Quantity Rational [u| deg |]
            lat'' = convert lat' :: Quantity Rational [u| rad |]
            lng'' = convert lng' :: Quantity Rational [u| rad |]

zoneToCylinder :: Raw.RawZone -> Zone
zoneToCylinder z =
    Cylinder radius (toLL(lat, lng))
    where
        radius = Radius (MkQuantity $ Raw.radius z % 1)
        RawLat lat = Raw.lat z
        RawLng lng = Raw.lng z

fixToPoint :: Kml.Fix -> Zone
fixToPoint fix =
    Point (toLL (lat, lng))
    where
        Kml.Latitude lat = Kml.lat fix
        Kml.Longitude lng = Kml.lng fix

crossedZone :: Zone -> [Zone] -> ZoneHit
crossedZone z xs =
    case entersZone z xs of
        e@(ZoneEnter _ _) -> e
        _ -> exitsZone z xs

entersZone :: Zone -> [Zone] -> ZoneHit
entersZone z xs =
    case exitsZone z $ reverse xs of
        ZoneExit x y -> ZoneEnter y x
        _ -> ZoneMiss

exitsZone :: Zone -> [Zone] -> ZoneHit
exitsZone z xs =
    case (insideZone, outsideZone) of
        (Just x, Just y) -> ZoneExit x y
        _ -> ZoneMiss
    where
        insideZone :: Maybe Int
        insideZone =
            List.findIndex (\y -> not $ Tsk.separatedZones [y, z]) xs

        outsideZone :: Maybe Int
        outsideZone =
            List.findIndex (\y -> Tsk.separatedZones [y, z]) xs

-- | A pilot has launched if their tracklog has distinct fixes.
launched :: Masking Bool
launched _ _ Kml.MarkedFixes{fixes} =
    not . null . nub $ fixes

started :: Masking Bool
started tasks (IxTask i) Kml.MarkedFixes{fixes} =
    case tasks ^? element (i - 1) of
        Nothing -> False
        Just Cmp.Task{speedSection, zones} ->
            case slice speedSection zones of
                [] ->
                    False

                z : _ ->
                    let ez = exitsZone (zoneToCylinder z) (fixToPoint <$> fixes)
                    in case ez of
                         ZoneExit _ _ -> True
                         _ -> False

madeGoal :: Masking Bool
madeGoal tasks (IxTask i) Kml.MarkedFixes{fixes} =
    case tasks ^? element (i - 1) of
        Nothing -> False
        Just Cmp.Task{zones} ->
            case reverse zones of
                [] ->
                    False

                z : _ ->
                    let ez = entersZone (zoneToCylinder z) (fixToPoint <$> fixes)
                    in case ez of
                         ZoneEnter _ _ -> True
                         _ -> False

tickedZones :: [Zone] -> [Zone] -> [ZoneHit]
tickedZones zones xs =
    flip crossedZone xs <$> zones

fixFromFix :: UTCTime -> Kml.Fix -> Cmp.Fix
fixFromFix mark0 x =
    -- SEE: https://ocharles.org.uk/blog/posts/2013-12-15-24-days-of-hackage-time.html
    Cmp.Fix { time = (fromInteger secs) `addUTCTime` mark0
            , lat = RawLat lat
            , lng = RawLng lng
            }
    where
        Kml.Seconds secs = Kml.mark x
        Kml.Latitude lat = Kml.lat x
        Kml.Longitude lng = Kml.lng x

proof :: [Kml.Fix] -> UTCTime -> Int -> Int -> [Bool] -> Maybe ZoneProof
proof fixes mark0 i j bs = do
    fixM <- fixes ^? element i
    fixN <- fixes ^? element j
    let fs = fixFromFix mark0 <$> [fixM, fixN]
    return $ ZoneProof { fixes = fs
                       , inZone = bs
                       }

madeZones :: [Cmp.Task]
          -> IxTask
          -> Kml.MarkedFixes
          -> ([Bool], [Maybe ZoneProof])
madeZones tasks (IxTask i) Kml.MarkedFixes{mark0, fixes} =
    case tasks ^? element (i - 1) of
        Nothing ->
            ([], [])

        Just Cmp.Task{zones} ->
            ((/= ZoneMiss) <$> xs, f <$> xs)
            where
                xs =
                    tickedZones
                        (zoneToCylinder <$> zones)
                        (fixToPoint <$> fixes)

                f :: ZoneHit -> Maybe ZoneProof
                f ZoneMiss = Nothing
                f (ZoneExit m n) = proof fixes mark0 m n [True, False]
                f (ZoneEnter m n) = proof fixes mark0 m n [False, True]

madeSpeedZones :: [Cmp.Task]
               -> IxTask
               -> Kml.MarkedFixes
               -> ([Bool], [Maybe ZoneProof])
madeSpeedZones tasks (IxTask i) Kml.MarkedFixes{mark0, fixes} =
    case tasks ^? element (i - 1) of
        Nothing ->
            ([], [])

        Just Cmp.Task{speedSection, zones} ->
            ((/= ZoneMiss) <$> xs, f <$> xs)
            where
                xs =
                    tickedZones
                        (zoneToCylinder <$> slice speedSection zones)
                        (fixToPoint <$> fixes)

                f :: ZoneHit -> Maybe ZoneProof
                f ZoneMiss = Nothing
                f (ZoneExit m n) = proof fixes mark0 m n [True, False]
                f (ZoneEnter m n) = proof fixes mark0 m n [False, True]

mm30 :: Tolerance
mm30 = Tolerance $ 30 % 1000

distanceViaZones :: (a -> Zone)
                 -> Cmp.SpeedSection
                 -> [Zone]
                 -> [a]
                 -> Maybe TaskDistance
distanceViaZones mkZone speedSection zs xs =
    case reverse xs of
        [] -> Nothing
        -- TODO: Check all fixes from last turnpoint made.
        x : _ ->
            Just . edges $
                distanceEdgeToEdge
                    PathPointToZone
                    mm30
                    (mkZone x : notTicked)
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        ys = (/= ZoneMiss) <$> tickedZones zsSpeed (mkZone <$> xs)
        notTicked = drop (length $ takeWhile (== True) ys) zsSpeed

slice :: Cmp.SpeedSection -> [a] -> [a]
slice = \case
    Nothing -> id
    Just (s', e') ->
        let (s, e) = (fromInteger s' - 1, fromInteger e' - 1)
        in take (e - s + 1) . drop s

distanceToGoal :: [Cmp.Task]
               -> IxTask
               -> Kml.MarkedFixes
               -> Maybe TaskDistance
distanceToGoal tasks (IxTask i) Kml.MarkedFixes{fixes} =
    case tasks ^? element (i - 1) of
        Nothing -> Nothing
        Just Cmp.Task{speedSection, zones} ->
            if null zones then Nothing else
            distanceViaZones
                fixToPoint
                speedSection
                (zoneToCylinder <$> zones)
                fixes 

distanceFlown :: [Cmp.Task]
              -> IxTask
              -> Kml.MarkedFixes
              -> Maybe PilotDistance
distanceFlown tasks (IxTask i) Kml.MarkedFixes{fixes} =
    case tasks ^? element (i - 1) of
        Nothing -> Nothing
        Just Cmp.Task{speedSection, zones} ->
            if null zones then Nothing else
            let cs = zoneToCylinder <$> zones
                d = distanceViaZones fixToPoint speedSection cs fixes
            in flownDistance speedSection cs d

flownDistance :: Cmp.SpeedSection
              -> [Zone]
              -> Maybe TaskDistance
              -> Maybe PilotDistance
flownDistance _ [] (Just (TaskDistance (MkQuantity d))) =
    Just $ PilotDistance d
flownDistance _ _ Nothing = Nothing
flownDistance speedSection zs@(z : _) (Just (TaskDistance d)) =
    case total of
        Nothing ->
            Nothing

        Just (TaskDistance dMax) ->
            Just . PilotDistance . unQuantity $ dMax -: d
    where
        total = distanceViaZones id speedSection zs [z]

timeFlown :: [Cmp.Task] -> IxTask -> Kml.MarkedFixes -> Maybe PilotTime
timeFlown tasks iTask@(IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing -> Nothing
        Just Cmp.Task{speedSection, zones, zoneTimes, startGates} ->
            if null zones || not atGoal then Nothing else
            let cs = zoneToCylinder <$> zones
            in flownDuration speedSection cs zoneTimes startGates xs
    where
        atGoal = madeGoal tasks iTask xs

flownDuration :: Cmp.SpeedSection
              -> [Zone]
              -> [Cmp.OpenClose]
              -> [Cmp.StartGate]
              -> Kml.MarkedFixes
              -> Maybe PilotTime
flownDuration speedSection zs os gs Kml.MarkedFixes{mark0, fixes}
    | null zs = Nothing
    | null fixes = Nothing
    | otherwise =
        durationViaZones fixToPoint Kml.mark speedSection zs os gs mark0 fixes

durationViaZones :: (Kml.Fix -> Zone)
                 -> (Kml.Fix -> Kml.Seconds)
                 -> Cmp.SpeedSection
                 -> [Zone]
                 -> [Cmp.OpenClose]
                 -> [Cmp.StartGate]
                 -> UTCTime
                 -> [Kml.Fix]
                 -> Maybe PilotTime
durationViaZones mkZone atTime speedSection zs os gs t0 xs =
    if null xs then Nothing else
    case (osSpeed, zsSpeed, reverse zsSpeed) of
        ([], _, _) -> Nothing
        (_, [], _) -> Nothing
        (_, _, []) -> Nothing
        (o0 : _, z0 : _, zN : _) -> duration o0 (z0, zN) xys
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        osSpeed =
            -- NOTE: When there is only one open/close all zones
            -- have the same open/close.
            case os of
                [_] -> os
                _ -> slice speedSection os

        xys :: [(Kml.Fix, (Zone, Zone))]
        xys = (\(x, y) -> (y, (mkZone x, mkZone y))) <$> zip (drop 1 xs) xs

        slots :: (Zone, Zone)
              -> [(Kml.Fix, (Zone, Zone))]
              -> (Maybe Kml.Seconds, Maybe Kml.Seconds)
        slots (z0, zN) xzs =
            (f <$> xz0, f <$> xzN)
            where
                exits' :: (Kml.Fix, (Zone, Zone)) -> Bool
                exits' (_, (zx, zy)) =
                    case exitsZone z0 [zx, zy] of
                        ZoneExit _ _ -> True
                        _ -> False

                enters' :: (Kml.Fix, (Zone, Zone)) -> Bool
                enters' (_, (zx, zy)) =
                    case entersZone zN [zx, zy] of
                        ZoneEnter _ _ -> True
                        _ -> False

                xz0 :: Maybe (Kml.Fix, (Zone, Zone))
                xz0 = List.find exits' xzs

                xzN :: Maybe (Kml.Fix, (Zone, Zone))
                xzN = List.find enters' xzs

                f = atTime . fst

        duration o z xzs =
            case slots z xzs of
                (Nothing, _) -> Nothing
                (_, Nothing) -> Nothing
                (Just s0, Just sN) ->
                    Just . PilotTime $ (deltaFlying + deltaStart) % 1
                    where
                        gs' = reverse gs
                        laterStart (Cmp.StartGate g) = g > t0

                        startTime =
                            case dropWhile laterStart gs' of
                                [] -> Cmp.open o
                                (Cmp.StartGate t : _) -> t

                        deltaStart :: Integer
                        deltaStart =
                            round $ diffUTCTime t0 startTime

                        (Kml.Seconds deltaFlying) = sN - s0
