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
    , ZoneEntry(..)
    , ZoneExit(..)
    , Crossing
    , CrossingPredicate
    , TaskZone(..)
    , TrackZone(..)
    , Ticked(..)
    , OrdCrossing(..)
    , slice
    , fixToPoint
    , zoneToCylinder
    , isStartExit
    , pickCrossingPredicate
    , fixFromFix
    , tickedZones
    , entersSeq
    , exitsSeq
    , distanceViaZones
    , distanceToGoal
    ) where

import Prelude hiding (span)
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.List (nub, sort)
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

-- | When working out distances around a course, if I know which zones are
-- tagged then I can break up the track into legs and assume previous legs are
-- ticked when working out distance to goal.
newtype Ticked = Ticked Int deriving (Eq, Show)

type ZoneIdx = Int

data ZoneEntry = ZoneEntry ZoneIdx ZoneIdx deriving (Eq, Show)
data ZoneExit = ZoneExit ZoneIdx ZoneIdx deriving (Eq, Show)
type Crossing = Either ZoneEntry ZoneExit

newtype OrdCrossing = OrdCrossing { unOrdCrossing :: Crossing }

index :: OrdCrossing -> Int
index (OrdCrossing (Left (ZoneEntry i _))) = i
index (OrdCrossing (Right (ZoneExit i _))) = i

instance Eq OrdCrossing where
    x == y = index x == index y

instance Ord OrdCrossing where
    compare x y = compare (index x) (index y)

instance Show OrdCrossing where
    show x = show $ index x

-- | A function that tests whether a flight track, represented as a series of point
-- zones crosses a zone.
type CrossingPredicate a b
    = TaskZone a
    -- ^ The task control zone.
    -> [TrackZone a]
    -- ^ The flight track represented as a series of point zones.
    -> [b]

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

zoneSingle :: (span -> zone -> [x] -> Maybe Int)
           -> (span -> zone -> [x] -> Maybe Int)
           -> (Int -> Int -> crossing)
           -> span
           -> zone
           -> [x]
           -> [crossing]
zoneSingle f g ctor span z xs =
    case g span z xs of
        Nothing -> []
        Just j ->
            case f span z . reverse $ ys of
                Just 0 -> [ctor (j - 1) j]
                _ -> []
            where
                ys = take j xs

-- | Finds the first pair of points, one outside the zone and the next inside.
-- Searches the fixes in order.
entersSingle :: (Real a, Fractional a)
             => SpanLatLng a
             -> CrossingPredicate a ZoneEntry
entersSingle =
    zoneSingle outsideZone insideZone ZoneEntry

-- | Finds the first pair of points, one inside the zone and the next outside.
-- Searches the fixes in order.
exitsSingle :: (Real a, Fractional a)
            => SpanLatLng a
            -> CrossingPredicate a ZoneExit
exitsSingle  =
    zoneSingle insideZone outsideZone ZoneExit

reindex :: Int -- ^ The length of the track, the number of fixes
        -> Either ZoneEntry ZoneExit
        -> Either ZoneEntry ZoneExit
reindex n (Right (ZoneExit i j)) =
    Right $ ZoneExit (i + n) (j + n)

reindex n (Left (ZoneEntry i j)) =
    Left $ ZoneEntry (i + n) (j + n)

crossSeq :: (Real a, Fractional a)
         => SpanLatLng a
         -> CrossingPredicate a Crossing
crossSeq span z xs =
    unOrdCrossing <$> (nub . sort $ enters ++ exits)
    where
        enters = OrdCrossing <$> entersSeq span z xs
        exits = OrdCrossing <$> exitsSeq span z xs

-- | Find the sequence of @take _ [entry, exit, .., entry, exit]@ going forward.
entersSeq :: (Real a, Fractional a)
          => SpanLatLng a
          -> CrossingPredicate a Crossing
entersSeq span z xs =
    case entersSingle span z xs of
        [] ->
            []

        (hit@(ZoneEntry _ j) : _) ->
            Left hit : (reindex j <$> exitsSeq span z (drop j xs))

-- | Find the sequence of @take _ [exit, entry.., exit, entry]@ going forward.
exitsSeq :: (Real a, Fractional a)
         => SpanLatLng a
         -> CrossingPredicate a Crossing
exitsSeq span z xs =
    case exitsSingle span z xs of
        [] ->
            []

        (hit@(ZoneExit _ j) : _) ->
            Right hit : (reindex j <$> entersSeq span z (drop j xs))

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

-- | Some pilots track logs will have initial values way off from the location
-- of the device. I suspect that the GPS logger is remembering the position it
-- had when last turned off, most likely at the end of yesterday's flight,
-- somewhere near where the pilot landed that day. Until the GPS receiver gets
-- a satellite fix and can compute the current position the stale, last known,
-- position gets logged. This means that a pilot may turn on their instrument
-- inside the start circle but their tracklog will start outside of it. For
-- this reason the crossing predicate is @crossSeq@ for all zones.
--
-- An example of a track log with this problem ...
--
-- 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 
-- 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 
-- 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 
-- 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 
-- 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 
-- 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 148.505133,-32.764317,0 
-- 148.505133,-32.764317,0 147.913967,-33.363200,448 147.913883,-33.363433,448 147.913817,-33.363633,448 147.913400,-33.364217,448 
pickCrossingPredicate
    :: (Real a, Fractional a)
    => SpanLatLng a
    -> Bool -- ^ Is the start an exit cylinder?
    -> Cmp.Task
    -> [CrossingPredicate a Crossing]
pickCrossingPredicate span _ Cmp.Task{zones} =
    const (crossSeq span) <$> zones

hitZone :: _ -> CrossingPredicate a Crossing
hitZone hit _ _ = [hit]

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

tickedZones :: [CrossingPredicate a b]
            -> [TaskZone a] -- ^ The control zones of the task.
            -> [TrackZone a] -- ^ The flown track.
            -> [[b]]
tickedZones fs zones xs =
    zipWith (\f z -> f z xs) fs zones

type DistanceViaZones a b c
    = (a -> TrackZone b)
    -> Cmp.SpeedSection
    -> [CrossingPredicate b c]
    -> [TaskZone b]
    -> [a]
    -> Maybe (TaskDistance b)

distanceToGoal :: (Real b, Fractional b)
               => SpanLatLng b
               -> (Raw.RawZone -> TaskZone b)
               -> DistanceViaZones _ _ _
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
                 => Ticked
                 -> SpanLatLng b
                 -> DistancePointToPoint b
                 -> CostSegment b
                 -> CircumSample b
                 -> AngleCut b
                 -> (a -> TrackZone b)
                 -> Cmp.SpeedSection
                 -> [CrossingPredicate b Crossing]
                 -> [TaskZone b]
                 -> [a]
                 -> Maybe (TaskDistance b)
distanceViaZones (Ticked n) span dpp cseg cs cut mkZone speedSection fs zs xs =
    case reverse xs of
        [] ->
            Nothing

        -- NOTE: I don't consider all fixes from last turnpoint made
        -- so this distance is the distance from the very last fix when
        -- at times on this leg the pilot may have been closer to goal.
        x : _ ->
            Just . edgesSum
            $ distanceEdgeToEdge span dpp cseg cs cut mm30 (cons x)
    where
        -- NOTE: Free pass for zones already ticked.
        fsTicked = const (hitZone . Left $ ZoneEntry 0 0) <$> ([0 .. ] :: [Int])

        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        fsSpeed = take n fsTicked ++ drop n (slice speedSection fs)

        ys :: [Bool]
        ys = (not . null) <$> tickedZones fsSpeed zsSpeed xs'

        numTicked = length $ takeWhile (== True) ys

        notTicked = drop numTicked zsSpeed

        zsNotTicked :: [Zone b]
        zsNotTicked = unTaskZone <$> notTicked

        xs' :: [TrackZone b]
        xs' = mkZone <$> xs

        cons :: a -> [Zone b]
        cons x = unTrackZone (mkZone x) : zsNotTicked
