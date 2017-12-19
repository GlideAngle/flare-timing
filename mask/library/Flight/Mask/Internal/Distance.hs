{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Mask.Internal.Distance
    ( Sliver(..)
    , Ticked
    , RaceSections(..)
    , section
    , distanceViaZones
    , distanceToGoal
    ) where

import Prelude hiding (span)
import Data.Ratio ((%))

import qualified Flight.Kml as Kml (MarkedFixes(..))
import Flight.Zone (Zone(..))
import qualified Flight.Zone.Raw as Raw (RawZone(..))
import qualified Flight.Comp as Cmp (Task(..), SpeedSection)
import Flight.Units ()
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Task
    ( Tolerance(..)
    , SpanLatLng
    , CostSegment
    , DistancePointToPoint
    , AngleCut(..)
    , CircumSample
    , distanceEdgeToEdge
    )
import Flight.Mask.Internal.Zone
import Flight.Mask.Internal.Cross
    (CrossingPredicate, crossingPredicates, isStartExit)

mm30 :: Fractional a => Tolerance a
mm30 = Tolerance . fromRational $ 30 % 1000

-- | When working out distances around a course, if I know which zones are
-- tagged then I can break up the track into legs and assume previous legs are
-- ticked when working out distance to goal.
type Ticked = RaceSections ZoneIdx

data RaceSections a =
    RaceSections
        { prolog :: [a]
        -- ^ Zones crossed before the start of the speed section.
        , race :: [a]
        -- ^ Zones crossed during the speed section.
        , epilog :: [a]
        -- ^ Zones crossed after the end of the speed section.
        }

data Sliver a =
    Sliver
        { span :: SpanLatLng a
        , dpp :: DistancePointToPoint a
        , cseg :: CostSegment a
        , cs :: CircumSample a
        , cut :: AngleCut a
        }

type DistanceViaZones a b c
    = (a -> TrackZone b)
    -> Cmp.SpeedSection
    -> [CrossingPredicate b c]
    -> [TaskZone b]
    -> [a]
    -> Maybe (TaskDistance b)

-- | Slice a list into three parts, before, during and after the speed section.
section :: Cmp.SpeedSection -> [a] -> RaceSections a 

section Nothing xs =
    RaceSections 
        { prolog = []
        , race = xs
        , epilog = []
        }

section (Just (s', e')) xs =
    RaceSections 
        { prolog = take s xs
        , race = take (e - s + 1) . drop s $ xs
        , epilog = drop (e + 1) xs
        }
    where
        (s, e) = (fromInteger s' - 1, fromInteger e' - 1)


distanceToGoal :: (Real b, Fractional b)
               => SpanLatLng b
               -> (Raw.RawZone -> TaskZone b)
               -> DistanceViaZones _ _ _
               -> Cmp.Task
               -> Kml.MarkedFixes
               -> Maybe (TaskDistance b)
               -- ^ Nothing indicates no such task or a task with no zones.
distanceToGoal
    span zoneToCyl dvz task@Cmp.Task{speedSection, zones} Kml.MarkedFixes{fixes} =
    if null zones then Nothing else
    dvz
        fixToPoint
        speedSection
        fs
        (zoneToCyl <$> zones)
        fixes 
    where
        fs =
            (\x ->
                crossingPredicates
                    span
                    (isStartExit span zoneToCyl x)
                    x)
            task

-- | A task is to be flown via its control zones. This function finds the last
-- leg made. The next leg is partial. Along this, the track fixes are checked
-- to find the one closest to the next zone at the end of the leg. From this the
-- distance returned is the task distance up to the next zone not made minus the
-- distance yet to fly to this zone.
distanceViaZones
    :: forall a b. (Real b, Fractional b)
    => Ticked -- ^ The number of zones ticked in the speed section
    -> Sliver b
    -> (a -> TrackZone b)
    -> Cmp.SpeedSection
    -> [CrossingPredicate b Crossing]
    -> [TaskZone b]
    -> [a]
    -> Maybe (TaskDistance b)
distanceViaZones ticked sliver mkZone speedSection fs zs xs =
    distanceViaZonesR ticked sliver mkZone speedSection fs zs (reverse xs)

-- | Distance via zones with the fixes reversed.
distanceViaZonesR
    :: forall a b. (Real b, Fractional b)
    => Ticked -- ^ The number of zones ticked in the speed section
    -> Sliver b
    -> (a -> TrackZone b)
    -> Cmp.SpeedSection
    -> [CrossingPredicate b Crossing]
    -> [TaskZone b]
    -> [a]
    -> Maybe (TaskDistance b)
distanceViaZonesR _ _ _ _ _ _ [] =
    Nothing

distanceViaZonesR
    RaceSections{race = []} Sliver{..} mkZone speedSection _ zs (x : _) =
    -- NOTE: Didn't make the start so skip the start.
    Just . edgesSum
    $ distanceEdgeToEdge span dpp cseg cs cut mm30 (cons mkZone x zsSkipStart)
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        zsSkipStart = unTaskZone <$> drop 1 zsSpeed

distanceViaZonesR
    RaceSections{race} Sliver{..} mkZone speedSection _ zs (x : _) =
    -- NOTE: I don't consider all fixes from last turnpoint made
    -- so this distance is the distance from the very last fix when
    -- at times on this leg the pilot may have been closer to goal.
    Just . edgesSum
    $ distanceEdgeToEdge span dpp cseg cs cut mm30 (cons mkZone x zsNotTicked)

    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        zsNotTicked = unTaskZone <$> drop (length race) zsSpeed

cons :: (a -> TrackZone b) -> a -> [Zone b] -> [Zone b]
cons mkZone x zs = unTrackZone (mkZone x) : zs
