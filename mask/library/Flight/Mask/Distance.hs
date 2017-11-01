{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Flight.Mask.Distance
    ( distanceToGoal
    , distancesToGoal
    , distanceFlown
    ) where

import Data.Time.Clock (UTCTime)
import Data.List (inits)
import Data.UnitsOfMeasure ((-:))
import Data.UnitsOfMeasure.Internal (Quantity(..), unQuantity)
import Control.Lens ((^?), element)

import qualified Flight.Kml as Kml (MarkedFixes(..), Fix)
import Flight.Track.Cross (Fix(..))
import qualified Flight.Comp as Cmp (Task(..), SpeedSection)
import Flight.TrackLog (IxTask(..))
import Flight.Task (TaskDistance(..))
import Flight.Score (PilotDistance(..))
import Flight.Units ()
import Flight.Mask.Internal
    ( TaskZone(..)
    , TrackZone(..)
    , CrossingPredicate
    , fixToPoint
    , zoneToCylinder
    , isStartExit
    , pickCrossingPredicate
    , fixFromFix
    , distanceViaZones
    )
import qualified Flight.Mask.Internal as I (distanceToGoal)
import qualified Flight.Zone.Raw as Raw (RawZone(..))

distancesToGoal :: Real a
                => (Raw.RawZone -> TaskZone a)
                -> [Cmp.Task]
                -> IxTask
                -> Kml.MarkedFixes
                -> Maybe [(Maybe Fix, Maybe TaskDistance)]
                -- ^ Nothing indicates no such task or a task with no zones.
distancesToGoal zoneToCyl tasks iTask@(IxTask i) Kml.MarkedFixes{mark0, fixes} =
    case tasks ^? element (i - 1) of
        Nothing -> Nothing
        Just Cmp.Task{zones} ->
            if null zones then Nothing else Just
            $ lastFixToGoal zoneToCyl tasks iTask mark0
            <$> inits fixes

-- | The distance from the last fix to goal passing through the remaining
-- control zones.
lastFixToGoal :: Real a
              => (Raw.RawZone -> TaskZone a)
              -> [Cmp.Task]
              -> IxTask
              -> UTCTime
              -> [Kml.Fix]
              -> (Maybe Fix, Maybe TaskDistance)
lastFixToGoal zoneToCyl tasks iTask mark0 ys =
    case reverse ys of
        [] -> (Nothing, Nothing)
        (y : _) -> (Just $ fixFromFix mark0 y, d)
    where
        xs = Kml.MarkedFixes { Kml.mark0 = mark0, Kml.fixes = ys }
        d = I.distanceToGoal zoneToCyl distanceViaZones tasks iTask xs

distanceToGoal :: Real a
               => (Raw.RawZone -> TaskZone a)
               -> [Cmp.Task]
               -> IxTask
               -> Kml.MarkedFixes
               -> Maybe TaskDistance
distanceToGoal zoneToCyl = I.distanceToGoal zoneToCyl distanceViaZones

distanceFlown :: (Real a, Fractional a)
              => (Raw.RawZone -> TaskZone a)
              -> [Cmp.Task]
              -> IxTask
              -> Kml.MarkedFixes
              -> Maybe (PilotDistance a)
distanceFlown zoneToCyl tasks (IxTask i) Kml.MarkedFixes{fixes} =
    case tasks ^? element (i - 1) of
        Nothing ->
            Nothing

        Just task@Cmp.Task{speedSection, zones} ->
            if null zones then Nothing else go speedSection fs cs d
            where
                fs = (\x -> pickCrossingPredicate (isStartExit zoneToCyl x) x) task
                cs = zoneToCylinder <$> zones
                d = distanceViaZones fixToPoint speedSection fs cs fixes

    where
        go :: (Real a, Fractional a)
           => Cmp.SpeedSection
           -> [CrossingPredicate a]
           -> [TaskZone a]
           -> Maybe TaskDistance
           -> Maybe (PilotDistance a)

        go _ _ [] (Just (TaskDistance (MkQuantity d))) =
            Just . PilotDistance $ fromRational d

        go _ _ _ Nothing =
            Nothing

        go speedSection fs zs@(z : _) (Just (TaskDistance d)) =
            case total of
                Nothing ->
                    Nothing

                Just (TaskDistance dMax) ->
                    Just . PilotDistance . fromRational . unQuantity $ dMax -: d
            where
                zoneToZone (TaskZone x) = TrackZone x
                total = distanceViaZones zoneToZone speedSection fs zs [z]
