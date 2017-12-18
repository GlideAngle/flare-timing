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
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Control.Lens ((^?), element)

import qualified Flight.Kml as Kml (MarkedFixes(..), Fix)
import Flight.Track.Cross (Fix(..))
import qualified Flight.Comp as Cmp (Task(..))
import Flight.TrackLog (IxTask(..))
import Flight.Score (PilotDistance(..))
import Flight.Units ()
import Flight.Mask.Internal (TaskZone(..), Ticked, fixFromFix, distanceViaZones)
import qualified Flight.Mask.Internal as I (distanceToGoal)
import qualified Flight.Zone.Raw as Raw (RawZone(..))
import Flight.Distance (TaskDistance(..))
import Flight.Task
    (SpanLatLng, CostSegment, DistancePointToPoint, AngleCut(..), CircumSample)

distancesToGoal :: (Real a, Fractional a)
                => Ticked
                -> SpanLatLng a
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
    ticked
    span dpp cseg cs cut
    zoneToCyl tasks iTask@(IxTask i) Kml.MarkedFixes{mark0, fixes} =
    case tasks ^? element (i - 1) of
        Nothing -> Nothing
        Just Cmp.Task{zones} ->
            -- NOTE: A ghci session using inits & tails.
            -- inits [1 .. 4]
            -- [[],[1],[1,2],[1,2,3],[1,2,3,4]]
            --
            -- tails [1 .. 4]
            -- [[1,2,3,4],[2,3,4],[3,4],[4],[]]
            --
            -- tails $ reverse [1 .. 4]
            -- [[4,3,2,1],[3,2,1],[2,1],[1],[]]
            --
            -- drop 1 $ inits [1 .. 4]
            -- [[1],[1,2],[1,2,3],[1,2,3,4]]
            if null zones then Nothing else Just
            $ lfg zoneToCyl tasks iTask mark0
            <$> drop 1 (inits fixes)
    where
        lfg = lastFixToGoal ticked span dpp cseg cs cut

-- | The distance from the last fix to goal passing through the remaining
-- control zones.
lastFixToGoal :: (Real a, Fractional a)
              => Ticked
              -> SpanLatLng a
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
    ticked
    span dpp cseg cs cut
    zoneToCyl tasks iTask mark0 ys =
    case reverse ys of
        [] -> (Nothing, Nothing)
        (y : _) -> (Just $ fixFromFix mark0 (length ys - 1) y, d)
    where
        xs = Kml.MarkedFixes { Kml.mark0 = mark0, Kml.fixes = ys }
        dvz = distanceViaZones ticked span dpp cseg cs cut
        d = I.distanceToGoal span zoneToCyl dvz tasks iTask xs

distanceToGoal :: (Real a, Fractional a)
               => Ticked
               -> SpanLatLng a
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
    ticked
    span dpp cseg cs cut
    zoneToCyl =
    I.distanceToGoal span zoneToCyl dvz
    where
        dvz = distanceViaZones ticked span dpp cseg cs cut

distanceFlown :: (Real a, Fractional a)
              => TaskDistance a
              -> Ticked
              -> SpanLatLng a
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
    (TaskDistance dTask)
    ticked
    span dpp cseg cs cut
    zoneToCyl tasks iTask@(IxTask i) fixes =
    case tasks ^? element (i - 1) of
        Nothing ->
            Nothing

        Just Cmp.Task{zones} ->
            if null zones then Nothing else do
                TaskDistance dPilot <-
                    distanceToGoal
                        ticked
                        span dpp cseg cs cut
                        zoneToCyl tasks iTask fixes

                let (MkQuantity diff) = dTask -: dPilot

                return $ PilotDistance diff
