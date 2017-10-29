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
    , DistanceViaZones
    , distanceViaZones
    , distanceToGoal
    ) where

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
    )
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng.Raw (RawLat(..), RawLng(..))
import Flight.Zone (Radius(..), Zone(..))
import qualified Flight.Zone.Raw as Raw (RawZone(..))
import Flight.Track.Cross (Fix(..))
import qualified Flight.Comp as Cmp (Task(..), SpeedSection)
import Flight.Units ()
import Flight.TrackLog (IxTask(..))
import qualified Flight.Kml as Kml (MarkedFixes(..))
import Flight.Task
    ( TaskDistance(..)
    , PathDistance(..)
    , Tolerance(..)
    , distanceEdgeToEdge
    , separatedZones
    )

mm30 :: Tolerance
mm30 = Tolerance $ 30 % 1000

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

slice :: Cmp.SpeedSection -> [a] -> [a]
slice = \case
    Nothing -> id
    Just (s', e') ->
        let (s, e) = (fromInteger s' - 1, fromInteger e' - 1)
        in take (e - s + 1) . drop s

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
insideZone (TaskZone z) =
    List.findIndex
        (\(TrackZone x) -> not $ separatedZones [x, z])

outsideZone :: TaskZone -> [TrackZone] -> Maybe Int
outsideZone (TaskZone z) =
    List.findIndex (\(TrackZone x) -> separatedZones [x, z])

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

pickCrossingPredicate
    :: Bool -- ^ Is the start an exit cylinder?
    -> Cmp.Task
    -> [CrossingPredicate]
pickCrossingPredicate False Cmp.Task{zones} =
    const entersZone <$> zones

pickCrossingPredicate True task@Cmp.Task{speedSection, zones} =
    case speedSection of
        Nothing ->
            pickCrossingPredicate False task

        Just (start, _) ->
            zipWith
                (\ i _ -> if i == start then exitsZone else entersZone)
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

tickedZones :: [CrossingPredicate]
            -> [TaskZone] -- ^ The control zones of the task.
            -> [TrackZone] -- ^ The flown track.
            -> [ZoneHit]
tickedZones fs zones xs =
    zipWith (\f z -> f z xs) fs zones

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

distanceToGoal :: DistanceViaZones
                -> [Cmp.Task]
                -> IxTask
                -> Kml.MarkedFixes
                -> Maybe TaskDistance

-- ^ Nothing indicates no such task or a task with no zones.
distanceToGoal dvz tasks (IxTask i) Kml.MarkedFixes{fixes} =
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

