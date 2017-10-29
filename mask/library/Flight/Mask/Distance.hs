{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Flight.Mask.Distance
    ( distanceToGoal
    , distancesToGoal
    , distanceFlown
    ) where

import Data.List (inits)
import Data.Ratio ((%))
import Data.UnitsOfMeasure ((-:))
import Data.UnitsOfMeasure.Internal (Quantity(..), unQuantity)
import Control.Lens ((^?), element)

import qualified Flight.Kml as Kml (MarkedFixes(..))
import Flight.Track.Cross (Fix(..))
import qualified Flight.Comp as Cmp (Task(..), SpeedSection)
import Flight.TrackLog as Log (IxTask(..))
import Flight.Task as Tsk
    ( TaskDistance(..)
    , PathDistance(..)
    , Tolerance(..)
    , distanceEdgeToEdge
    )
import Flight.Score as Gap (PilotDistance(..))
import Flight.Units ()
import Flight.Mask.Internal
    ( TaskZone(..)
    , TrackZone(..)
    , CrossingPredicate
    , ZoneHit(..)
    , slice
    , fixToPoint
    , zoneToCylinder
    , isStartExit
    , pickCrossingPredicate
    , fixFromFix
    , tickedZones
    )

mm30 :: Tolerance
mm30 = Tolerance $ 30 % 1000

type DistanceViaZones =
    forall a. (a -> TrackZone)
    -> Cmp.SpeedSection
    -> [CrossingPredicate]
    -> [TaskZone]
    -> [a]
    -> Maybe TaskDistance

-- | A task is to be flown via its control zones. This function finds the last
-- leg made. The next leg is partial. Along this, the track fixes are checked
-- to find the one closest to the next zone at the end of the leg. From this the
-- distance returned is the task distance up to the next zone not made minus the
-- distance yet to fly to this zone.
distanceViaZones :: DistanceViaZones
distanceViaZones mkZone speedSection fs zs xs =
    case reverse xs of
        [] ->
            Nothing

        -- TODO: Check all fixes from last turnpoint made.
        x : _ ->
            Just . edgesSum $
                distanceEdgeToEdge
                    mm30
                    (unTrackZone (mkZone x) : notTicked)
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        fsSpeed = slice speedSection fs
        ys = (/= ZoneMiss) <$> tickedZones fsSpeed zsSpeed (mkZone <$> xs)
        notTicked = unTaskZone <$> drop (length $ takeWhile (== True) ys) zsSpeed

distancesToGoal :: [Cmp.Task]
                -> IxTask
                -> Kml.MarkedFixes
                -> Maybe [(Maybe Fix, Maybe TaskDistance)]
                -- ^ Nothing indicates no such task or a task with no zones.
distancesToGoal tasks iTask@(IxTask i) mf@Kml.MarkedFixes{mark0, fixes} =
    case tasks ^? element (i - 1) of
        Nothing -> Nothing
        Just Cmp.Task{zones} ->
            if null zones then Nothing else Just $ f <$> fixes'
            where
                fixes' = inits fixes

                f ys =
                    case reverse ys of
                        [] -> (Nothing, Nothing)
                        (y : _) ->
                            (Just $ fixFromFix mark0 y, d)
                            where
                                xs = mf { Kml.fixes = ys }
                                d = distanceToGoal' distanceViaZones tasks iTask xs

distanceToGoal :: [Cmp.Task]
               -> IxTask
               -> Kml.MarkedFixes
               -> Maybe TaskDistance
distanceToGoal = distanceToGoal' distanceViaZones

distanceToGoal' :: DistanceViaZones
                -> [Cmp.Task]
                -> IxTask
                -> Kml.MarkedFixes
                -> Maybe TaskDistance

-- ^ Nothing indicates no such task or a task with no zones.
distanceToGoal' dvz tasks (IxTask i) Kml.MarkedFixes{fixes} =
    case tasks ^? element (i - 1) of
        Nothing -> Nothing
        Just task@Cmp.Task{speedSection, zones} ->
            if null zones then Nothing else
            dvz
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
