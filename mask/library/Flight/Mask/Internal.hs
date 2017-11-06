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
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Mask.Internal
    ( ZoneIdx
    , ZoneHit(..)
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
    , fixFromFix
    , tickedZones
    -- , DistanceViaZones
    , distanceViaZones
    , distanceToGoal
    ) where

import Prelude hiding (span)
import Data.Time.Clock (UTCTime, addUTCTime)
import qualified Data.List as List (findIndex)
import Data.Ratio ((%))
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Control.Lens ((^?), element)

import qualified Flight.Kml as Kml
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
import Flight.Track.Cross (Fix(..))
import qualified Flight.Comp as Cmp (Task(..), SpeedSection)
import Flight.Units ()
import Flight.TrackLog (IxTask(..))
import Flight.Task
    ( TaskDistance(..)
    , PathDistance(..)
    , Tolerance(..)
    , SpanLatLng
    , CostSegment
    , DistancePointToPoint
    , AngleCut(..)
    , CircumSample
    , distanceEdgeToEdge
    , separatedZones
    )

mm30 :: Fractional a => Tolerance a
mm30 = Tolerance . fromRational $ 30 % 1000

type ZoneIdx = Int

data ZoneHit
    = ZoneMiss
    | ZoneEntry ZoneIdx ZoneIdx
    | ZoneExit ZoneIdx ZoneIdx
    deriving Eq

-- | A function that tests whether a flight track, represented as a series of point
-- zones crosses a zone.
type CrossingPredicate a
    = TaskZone a
    -- ^ The task control zone.
    -> [TrackZone a]
    -- ^ The flight track represented as a series of point zones.
    -> ZoneHit

-- | A task control zone.
newtype TaskZone a = TaskZone { unTaskZone :: Zone a }

-- | A fix in a flight track converted to a point zone.
newtype TrackZone a = TrackZone { unTrackZone :: Zone a }

slice :: Cmp.SpeedSection -> [a] -> [a]
slice = \case
    Nothing -> id
    Just (s', e') ->
        let (s, e) = (fromInteger s' - 1, fromInteger e' - 1)
        in take (e - s + 1) . drop s

-- | The input pair is in degrees while the output is in radians.
toLL :: Fractional a => (Rational, Rational) -> LatLng a [u| rad |]
toLL (lat, lng) =
    LatLng (x', y')
    where
        lat' = MkQuantity lat :: Quantity Rational [u| deg |]
        lng' = MkQuantity lng :: Quantity Rational [u| deg |]

        (MkQuantity x) = convert lat' :: Quantity Rational [u| rad |]
        (MkQuantity y) = convert lng' :: Quantity Rational [u| rad |]

        x' = Lat . MkQuantity $ realToFrac x
        y' = Lng . MkQuantity $ realToFrac y

zoneToCylinder :: (Eq a, Fractional a) => Raw.RawZone -> TaskZone a
zoneToCylinder z =
    TaskZone $ Cylinder radius (toLL(lat, lng))
    where
        r = Raw.radius z
        r' = fromRational $ r % 1

        radius = Radius (MkQuantity r')
        RawLat lat = Raw.lat z
        RawLng lng = Raw.lng z

fixToPoint :: (Eq a, Fractional a) => Kml.Fix -> TrackZone a
fixToPoint fix =
    TrackZone $ Point (toLL (lat, lng))
    where
        Kml.Latitude lat = Kml.lat fix
        Kml.Longitude lng = Kml.lng fix

insideZone :: (Real a, Fractional a)
           => SpanLatLng a
           -> TaskZone a
           -> [TrackZone a]
           -> Maybe Int
insideZone span (TaskZone z) =
    List.findIndex (\(TrackZone x) -> not $ separatedZones span [x, z])

outsideZone :: (Real a, Fractional a)
            => SpanLatLng a
            -> TaskZone a
            -> [TrackZone a]
            -> Maybe Int
outsideZone span (TaskZone z) =
    List.findIndex (\(TrackZone x) -> separatedZones span [x, z])

-- | Finds the first pair of points, one outside the zone and the next inside.
entersZone :: (Real a, Fractional a) => SpanLatLng a -> CrossingPredicate a
entersZone span z xs =
    case insideZone span z xs of
        Nothing -> ZoneMiss
        Just j ->
            case outsideZone span z . reverse $ take j xs of
                Just 0 -> ZoneEntry (j - 1) j
                _ -> ZoneMiss

-- | Finds the first pair of points, one inside the zone and the next outside.
exitsZone :: (Real a, Fractional a) => SpanLatLng a -> CrossingPredicate a
exitsZone span z xs =
    case outsideZone span z xs of
        Nothing -> ZoneMiss
        Just j ->
            case insideZone span z . reverse $ take j xs of
                Just 0 -> ZoneExit (j - 1) j
                _ -> ZoneMiss

-- | A start zone is either entry or exit when all other zones are entry.
-- If I must fly into the start cylinder to reach the next turnpoint then
-- the start zone is entry otherwise it is exit. In one case the start cylinder
-- contains the next turnpoint and in the other the start cylinder is
-- completely separate from the next turnpoint.
isStartExit :: (Real a, Fractional a)
            => SpanLatLng a
            -> (Raw.RawZone
            -> TaskZone a)
            -> Cmp.Task
            -> Bool
isStartExit span zoneToCyl Cmp.Task{speedSection, zones} =
    case speedSection of
        Nothing ->
            False

        Just (ii, _) ->
            let i = fromInteger ii
            in case (zones ^? element (i - 1), zones ^? element i) of
                (Just start, Just tp1) ->
                    separatedZones span
                    $ unTaskZone . zoneToCyl
                    <$> [start, tp1]

                _ ->
                    False

pickCrossingPredicate
    :: (Real a, Fractional a)
    => SpanLatLng a
    -> Bool -- ^ Is the start an exit cylinder?
    -> Cmp.Task
    -> [CrossingPredicate a]
pickCrossingPredicate span False Cmp.Task{zones} =
    const (entersZone span) <$> zones

pickCrossingPredicate span True task@Cmp.Task{speedSection, zones} =
    case speedSection of
        Nothing ->
            pickCrossingPredicate span False task

        Just (start, _) ->
            zipWith
                (\ i _ ->
                    if i == start then exitsZone span else entersZone span)
                [1 .. ]
                zones

fixFromFix :: UTCTime -> Kml.Fix -> Fix
fixFromFix mark0 x =
    -- SEE: https://ocharles.org.uk/blog/posts/2013-12-15-24-days-of-hackage-time.html
    Fix { time = fromInteger secs `addUTCTime` mark0
        , lat = RawLat lat
        , lng = RawLng lng
        }
    where
        Kml.Seconds secs = Kml.mark x
        Kml.Latitude lat = Kml.lat x
        Kml.Longitude lng = Kml.lng x

tickedZones :: [CrossingPredicate a]
            -> [TaskZone a] -- ^ The control zones of the task.
            -> [TrackZone a] -- ^ The flown track.
            -> [ZoneHit]
tickedZones fs zones xs =
    zipWith (\f z -> f z xs) fs zones

type DistanceViaZones a b
    = (a -> TrackZone b)
    -> Cmp.SpeedSection
    -> [CrossingPredicate b]
    -> [TaskZone b]
    -> [a]
    -> Maybe (TaskDistance b)

distanceToGoal :: (Real b, Fractional b)
               => SpanLatLng b
               -> (Raw.RawZone -> TaskZone b)
               -> DistanceViaZones _ _
               -> [Cmp.Task]
               -> IxTask
               -> Kml.MarkedFixes
               -> Maybe (TaskDistance b)

-- ^ Nothing indicates no such task or a task with no zones.
distanceToGoal span zoneToCyl dvz tasks (IxTask i) Kml.MarkedFixes{fixes} =
    case tasks ^? element (i - 1) of
        Nothing -> Nothing
        Just task@Cmp.Task{speedSection, zones} ->
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
                        pickCrossingPredicate
                            span
                            (isStartExit span zoneToCyl x)
                            x)
                    task

-- | A task is to be flown via its control zones. This function finds the last
-- leg made. The next leg is partial. Along this, the track fixes are checked
-- to find the one closest to the next zone at the end of the leg. From this the
-- distance returned is the task distance up to the next zone not made minus the
-- distance yet to fly to this zone.
distanceViaZones :: forall a b. (Real b, Fractional b)
                 => SpanLatLng b
                 -> DistancePointToPoint b
                 -> CostSegment b
                 -> CircumSample b
                 -> AngleCut b
                 -> (a -> TrackZone b)
                 -> Cmp.SpeedSection
                 -> [CrossingPredicate b]
                 -> [TaskZone b]
                 -> [a]
                 -> Maybe (TaskDistance b)
distanceViaZones span dpp cseg cs cut mkZone speedSection fs zs xs =
    case reverse xs of
        [] ->
            Nothing

        -- TODO: Check all fixes from last turnpoint made.
        x : _ ->
            Just . edgesSum $
                distanceEdgeToEdge span dpp cseg cs cut mm30 (cons x)
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        fsSpeed = slice speedSection fs

        ys :: [Bool]
        ys = (/= ZoneMiss) <$> tickedZones fsSpeed zsSpeed (mkZone <$> xs)

        notTicked = unTaskZone <$> drop (length $ takeWhile (== True) ys) zsSpeed

        cons x = unTrackZone (mkZone x) : notTicked
