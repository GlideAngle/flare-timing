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
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Mask.Distance
    ( distanceToGoal
    , distancesToGoal
    , distanceFlown
    ) where

import Prelude hiding (span)
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
import Flight.Task
    ( SpanLatLng
    , CostSegment
    , DistancePointToPoint
    , AngleCut(..)
    , CircumSample
    )

distancesToGoal :: (Real a, Fractional a)
                => SpanLatLng a
                -> DistancePointToPoint a
                -> CostSegment a
                -> CircumSample a
                -> AngleCut a
                -> (Raw.RawZone -> TaskZone a)
                -> [Cmp.Task]
                -> IxTask
                -> Kml.MarkedFixes
                -> Maybe [(Maybe Fix, Maybe (TaskDistance a))]
                -- ^ Nothing indicates no such task or a task with no zones.
distancesToGoal
    span dpp cseg cs cut
    zoneToCyl tasks iTask@(IxTask i) Kml.MarkedFixes{mark0, fixes} =
    case tasks ^? element (i - 1) of
        Nothing -> Nothing
        Just Cmp.Task{zones} ->
            if null zones then Nothing else Just
            $ lfg zoneToCyl tasks iTask mark0
            <$> inits fixes
    where
        lfg = lastFixToGoal span dpp cseg cs cut

-- | The distance from the last fix to goal passing through the remaining
-- control zones.
lastFixToGoal :: (Real a, Fractional a)
              => SpanLatLng a
              -> DistancePointToPoint a
              -> CostSegment a
              -> CircumSample a
              -> AngleCut a
              -> (Raw.RawZone -> TaskZone a)
              -> [Cmp.Task]
              -> IxTask
              -> UTCTime
              -> [Kml.Fix]
              -> (Maybe Fix, Maybe (TaskDistance a))
lastFixToGoal
    span dpp cseg cs cut
    zoneToCyl tasks iTask mark0 ys =
    case reverse ys of
        [] -> (Nothing, Nothing)
        (y : _) -> (Just $ fixFromFix mark0 y, d)
    where
        xs = Kml.MarkedFixes { Kml.mark0 = mark0, Kml.fixes = ys }
        dvz = distanceViaZones span dpp cseg cs cut
        d = I.distanceToGoal span zoneToCyl dvz tasks iTask xs

distanceToGoal :: (Real a, Fractional a)
               => SpanLatLng a
               -> DistancePointToPoint a
               -> CostSegment a
               -> CircumSample a
               -> AngleCut a
               -> (Raw.RawZone -> TaskZone a)
               -> [Cmp.Task]
               -> IxTask
               -> Kml.MarkedFixes
               -> Maybe (TaskDistance a)
distanceToGoal
    span dpp cseg cs cut
    zoneToCyl =
    I.distanceToGoal span zoneToCyl (distanceViaZones span dpp cseg cs cut)

distanceFlown :: (Real a, Fractional a)
              => SpanLatLng a
              -> DistancePointToPoint a
              -> CostSegment a
              -> CircumSample a
              -> AngleCut a
              -> (Raw.RawZone -> TaskZone a)
              -> [Cmp.Task]
              -> IxTask
              -> Kml.MarkedFixes
              -> Maybe (PilotDistance a)
distanceFlown
    span dpp cseg cs cut
    zoneToCyl tasks (IxTask i) Kml.MarkedFixes{fixes} =
    case tasks ^? element (i - 1) of
        Nothing ->
            Nothing

        Just task@Cmp.Task{speedSection, zones} ->
            if null zones then Nothing else go speedSection fs cyls d
            where
                fs :: [CrossingPredicate _]
                fs =
                    (\x ->
                        let b = isStartExit span zoneToCyl x
                        in pickCrossingPredicate span b x) task

                cyls = zoneToCylinder <$> zones

                d :: Maybe (TaskDistance _)
                d =
                    distanceViaZones
                        span dpp cseg cs cut
                        fixToPoint speedSection fs cyls fixes

    where
        go :: Cmp.SpeedSection
           -> [CrossingPredicate _]
           -> [TaskZone _]
           -> Maybe (TaskDistance _)
           -> Maybe (PilotDistance _)

        go _ _ [] (Just (TaskDistance (MkQuantity d))) =
            Just . PilotDistance $ d

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
                total =
                    distanceViaZones
                        span dpp cseg cs cut
                        zoneToZone speedSection fs zs [z]
