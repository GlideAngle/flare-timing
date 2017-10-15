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
    | ZoneEntry ZoneIdx ZoneIdx
    | ZoneExit ZoneIdx ZoneIdx
    deriving Eq

-- | A function that tests whether a flight track, represented as a series of point
-- zones crosses a zone.
type CrossingPredicate
    = TaskZone -- ^ The task control zone.
    -> [TrackZone] -- ^ The flight track represented as a series of point zones.
    -> ZoneHit

-- | A task control zone.
newtype TaskZone = TaskZone { unTaskZone :: Zone }

-- | A fix in a flight track converted to a point zone.
newtype TrackZone = TrackZone { unTrackZone :: Zone }

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

zoneToCylinder :: Raw.RawZone -> TaskZone
zoneToCylinder z =
    TaskZone $ Cylinder radius (toLL(lat, lng))
    where
        radius = Radius (MkQuantity $ Raw.radius z % 1)
        RawLat lat = Raw.lat z
        RawLng lng = Raw.lng z

fixToPoint :: Kml.Fix -> TrackZone
fixToPoint fix =
    TrackZone $ Point (toLL (lat, lng))
    where
        Kml.Latitude lat = Kml.lat fix
        Kml.Longitude lng = Kml.lng fix

insideZone :: TaskZone -> [TrackZone] -> Maybe Int
insideZone (TaskZone z) xs =
    List.findIndex
        (\(TrackZone x) -> not $ Tsk.separatedZones [x, z]) xs

outsideZone :: TaskZone -> [TrackZone] -> Maybe Int
outsideZone (TaskZone z) xs =
    List.findIndex
        (\(TrackZone x) -> Tsk.separatedZones [x, z]) xs

-- | Finds the first pair of points, one outside the zone and the next inside.
entersZone :: CrossingPredicate
entersZone z xs =
    case insideZone z xs of
        Nothing -> ZoneMiss
        Just j ->
            case outsideZone z . reverse $ take j xs of
                Just 0 -> ZoneEntry (j - 1) j
                _ -> ZoneMiss

-- | Finds the first pair of points, one inside the zone and the next outside.
exitsZone :: CrossingPredicate
exitsZone z xs =
    case outsideZone z xs of
        Nothing -> ZoneMiss
        Just j ->
            case insideZone z . reverse $ take j xs of
                Just 0 -> ZoneExit (j - 1) j
                _ -> ZoneMiss

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
                         ZoneEntry _ _ -> True
                         _ -> False

-- | A start zone is either entry or exit when all other zones are entry.
-- If I must fly into the start cylinder to reach the next turnpoint then
-- the start zone is entry otherwise it is exit. In one case the start cylinder
-- contains the next turnpoint and in the other the start cylinder is
-- completely separate from the next turnpoint.
isStartExit :: Cmp.Task -> Bool
isStartExit Cmp.Task{speedSection, zones} =
    case speedSection of
        Nothing ->
            False

        Just (ii, _) ->
            let i = fromInteger ii
            in case (zones ^? element (i - 1), zones ^? element i) of
                (Just start, Just tp1) ->
                    separatedZones
                    $ unTaskZone . zoneToCylinder
                    <$> [start, tp1]

                _ ->
                    False

tickedZones :: [CrossingPredicate]
            -> [TaskZone] -- ^ The control zones of the task.
            -> [TrackZone] -- ^ The flown track.
            -> [ZoneHit]
tickedZones fs zones xs =
    zipWith (\f z -> f z xs) fs zones

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
    return $ ZoneProof { crossing = fs
                       , inZone = bs
                       }

-- | Given two points on either side of a zone, what is the crossing time.
crossingTime :: [Kml.Fix] -> UTCTime -> Int -> Int -> [Bool] -> Maybe UTCTime

crossingTime fixes mark0 i _ [True, False] = do
    -- TODO: Interpolate between crossing points. For now I just take the time
    -- of the point on the inside.
    fixM <- fixes ^? element i
    let Kml.Seconds secs = Kml.mark fixM
    return $ fromInteger secs `addUTCTime` mark0

crossingTime fixes mark0 _ j [False, True] = do
    fixN <- fixes ^? element j
    let Kml.Seconds secs = Kml.mark fixN
    return $ fromInteger secs `addUTCTime` mark0

crossingTime _ _ _ _ _ = Nothing

pickCrossingPredicate
    :: Bool -- ^ Is the start an exit cylinder?
    -> Cmp.Task
    -> [CrossingPredicate]
pickCrossingPredicate False Cmp.Task{zones} =
    (const entersZone) <$> zones

pickCrossingPredicate True task@Cmp.Task{speedSection, zones} =
    case speedSection of
        Nothing ->
            pickCrossingPredicate False task

        Just (start, _) ->
            zipWith
                (\ i _ -> if i == start then exitsZone else entersZone)
                [1 .. ]
                zones

madeZones :: [Cmp.Task]
          -> IxTask
          -> Kml.MarkedFixes
          -> ([Maybe UTCTime], [Maybe ZoneProof])
madeZones tasks (IxTask i) Kml.MarkedFixes{mark0, fixes} =
    case tasks ^? element (i - 1) of
        Nothing ->
            ([], [])

        Just task@Cmp.Task{zones} ->
            (f <$> xs, g <$> xs)
            where
                fs = (\x -> pickCrossingPredicate (isStartExit x) x) task

                xs =
                    tickedZones
                        fs
                        (zoneToCylinder <$> zones)
                        (fixToPoint <$> fixes)

                f :: ZoneHit -> Maybe UTCTime
                f ZoneMiss = Nothing
                f (ZoneExit m n) = crossingTime fixes mark0 m n [True, False]
                f (ZoneEntry m n) = crossingTime fixes mark0 m n [False, True]

                g :: ZoneHit -> Maybe ZoneProof
                g ZoneMiss = Nothing
                g (ZoneExit m n) = proof fixes mark0 m n [True, False]
                g (ZoneEntry m n) = proof fixes mark0 m n [False, True]

madeSpeedZones :: [Cmp.Task]
               -> IxTask
               -> Kml.MarkedFixes
               -> ([Maybe UTCTime], [Maybe ZoneProof])
madeSpeedZones tasks (IxTask i) Kml.MarkedFixes{mark0, fixes} =
    case tasks ^? element (i - 1) of
        Nothing ->
            ([], [])

        Just task@Cmp.Task{speedSection, zones} ->
            (f <$> xs, g <$> xs)
            where
                fs = (\x -> pickCrossingPredicate (isStartExit x) x) task

                xs =
                    tickedZones
                        (slice speedSection fs)
                        (zoneToCylinder <$> slice speedSection zones)
                        (fixToPoint <$> fixes)

                f :: ZoneHit -> Maybe UTCTime
                f ZoneMiss = Nothing
                f (ZoneExit m n) = crossingTime fixes mark0 m n [True, False]
                f (ZoneEntry m n) = crossingTime fixes mark0 m n [False, True]

                g :: ZoneHit -> Maybe ZoneProof
                g ZoneMiss = Nothing
                g (ZoneExit m n) = proof fixes mark0 m n [True, False]
                g (ZoneEntry m n) = proof fixes mark0 m n [False, True]

mm30 :: Tolerance
mm30 = Tolerance $ 30 % 1000

-- | A task is to be flown via its control zones. This function finds the last
-- leg made. The next leg is partial. Along this, the track fixes are checked
-- to find the one closest to the next zone at the end of the leg. From this the
-- distance returned is the task distance up to the next zone not made minus the
-- distance yet to fly to this zone.
distanceViaZones :: (a -> TrackZone)
                 -> Cmp.SpeedSection
                 -> [CrossingPredicate]
                 -> [TaskZone]
                 -> [a]
                 -> Maybe TaskDistance
distanceViaZones mkZone speedSection fs zs xs =
    case reverse xs of
        [] ->
            Nothing

        -- TODO: Check all fixes from last turnpoint made.
        x : _ ->
            Just . edges $
                distanceEdgeToEdge
                    PathPointToZone
                    mm30
                    (unTrackZone (mkZone x) : notTicked)
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        fsSpeed = slice speedSection fs
        ys = (/= ZoneMiss) <$> tickedZones fsSpeed zsSpeed (mkZone <$> xs)
        notTicked = unTaskZone <$> drop (length $ takeWhile (== True) ys) zsSpeed

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
        Just task@Cmp.Task{speedSection, zones} ->
            if null zones then Nothing else
            distanceViaZones
                fixToPoint
                speedSection
                fs
                (zoneToCylinder <$> zones)
                fixes 
            where
                fs = (\x -> pickCrossingPredicate (isStartExit x) x) task

distanceFlown :: [Cmp.Task]
              -> IxTask
              -> Kml.MarkedFixes
              -> Maybe PilotDistance
distanceFlown tasks (IxTask i) Kml.MarkedFixes{fixes} =
    case tasks ^? element (i - 1) of
        Nothing ->
            Nothing

        Just task@Cmp.Task{speedSection, zones} ->
            if null zones then Nothing else go speedSection fs cs d
            where
                fs = (\x -> pickCrossingPredicate (isStartExit x) x) task
                cs = zoneToCylinder <$> zones
                d = distanceViaZones fixToPoint speedSection fs cs fixes

    where
        go :: Cmp.SpeedSection
           -> [CrossingPredicate]
           -> [TaskZone]
           -> Maybe TaskDistance
           -> Maybe PilotDistance

        go _ _ [] (Just (TaskDistance (MkQuantity d))) =
            Just $ PilotDistance d

        go _ _ _ Nothing =
            Nothing

        go speedSection fs zs@(z : _) (Just (TaskDistance d)) =
            case total of
                Nothing ->
                    Nothing

                Just (TaskDistance dMax) ->
                    Just . PilotDistance . unQuantity $ dMax -: d
            where
                zoneToZone (TaskZone x) = TrackZone x
                total = distanceViaZones zoneToZone speedSection fs zs [z]

timeFlown :: [Cmp.Task] -> IxTask -> Kml.MarkedFixes -> Maybe PilotTime
timeFlown tasks iTask@(IxTask i) xs =
    case tasks ^? element (i - 1) of
        Nothing ->
            Nothing

        Just task@Cmp.Task{speedSection, zones, zoneTimes, startGates} ->
            if null zones || not atGoal then Nothing else
            flownDuration speedSection fs cs zoneTimes startGates xs
            where
                fs = (\x -> pickCrossingPredicate (isStartExit x) x) task
                cs = zoneToCylinder <$> zones

    where
        atGoal = madeGoal tasks iTask xs

flownDuration :: Cmp.SpeedSection
              -> [CrossingPredicate]
              -> [TaskZone]
              -> [Cmp.OpenClose]
              -> [Cmp.StartGate]
              -> Kml.MarkedFixes
              -> Maybe PilotTime
flownDuration speedSection fs zs os gs Kml.MarkedFixes{mark0, fixes}
    | null zs = Nothing
    | null fixes = Nothing
    | otherwise =
        durationViaZones fixToPoint Kml.mark speedSection fs zs os gs mark0 fixes

durationViaZones :: (Kml.Fix -> TrackZone)
                 -> (Kml.Fix -> Kml.Seconds)
                 -> Cmp.SpeedSection
                 -> [CrossingPredicate]
                 -> [TaskZone]
                 -> [Cmp.OpenClose]
                 -> [Cmp.StartGate]
                 -> UTCTime
                 -> [Kml.Fix]
                 -> Maybe PilotTime
durationViaZones mkZone atTime speedSection _ zs os gs t0 xs =
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

        xys :: [(Kml.Fix, (TrackZone, TrackZone))]
        xys = (\(x, y) -> (y, (mkZone x, mkZone y))) <$> zip (drop 1 xs) xs

        -- TODO: Account for entry start zone.
        slots :: (TaskZone, TaskZone)
              -> [(Kml.Fix, (TrackZone, TrackZone))]
              -> (Maybe Kml.Seconds, Maybe Kml.Seconds)
        slots (z0, zN) xzs =
            (f <$> xz0, f <$> xzN)
            where
                exits' :: (Kml.Fix, (TrackZone, TrackZone)) -> Bool
                exits' (_, (zx, zy)) =
                    case exitsZone z0 [zx, zy] of
                        ZoneExit _ _ -> True
                        _ -> False

                enters' :: (Kml.Fix, (TrackZone, TrackZone)) -> Bool
                enters' (_, (zx, zy)) =
                    case entersZone zN [zx, zy] of
                        ZoneEntry _ _ -> True
                        _ -> False

                xz0 :: Maybe (Kml.Fix, (TrackZone, TrackZone))
                xz0 = List.find exits' xzs

                xzN :: Maybe (Kml.Fix, (TrackZone, TrackZone))
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
