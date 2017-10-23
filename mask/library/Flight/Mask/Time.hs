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

module Flight.Mask.Time (timeFlown) where

import Data.Time.Clock (UTCTime, diffUTCTime)
import qualified Data.List as List (find)
import Data.Ratio ((%))
import Control.Lens ((^?), element)

import qualified Flight.Kml as Kml
    ( Fix
    , Seconds(..)
    , FixMark(..)
    , MarkedFixes(..)
    )
import qualified Flight.Comp as Cmp
    ( Task(..)
    , SpeedSection
    , OpenClose(..)
    , StartGate(..)
    )
import Flight.TrackLog as Log (IxTask(..))
import Flight.Score as Gap (PilotTime(..))
import Flight.Units ()
import Flight.Mask.Tag (madeGoal)
import Flight.Mask.Internal
    ( ZoneHit(..)
    , CrossingPredicate
    , TaskZone(..)
    , TrackZone(..)
    , slice
    , exitsZone
    , entersZone
    , fixToPoint
    , zoneToCylinder
    , isStartExit
    , pickCrossingPredicate
    )

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
