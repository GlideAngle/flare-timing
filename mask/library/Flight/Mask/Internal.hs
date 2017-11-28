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
    , exitsZoneFwd
    , exitsZoneRev
    , entersZoneFwd
    , entersZoneRev
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
import Data.Either (lefts, rights)
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
data Ticked = Ticked Int deriving (Eq, Show)

type ZoneIdx = Int

data ZoneEntry = ZoneEntry ZoneIdx ZoneIdx deriving (Eq, Show)
data ZoneExit = ZoneExit ZoneIdx ZoneIdx deriving (Eq, Show)
type Crossing = Either ZoneEntry ZoneExit

newtype OrdCrossing = OrdCrossing { unOrdCrossing :: Crossing }

index :: OrdCrossing -> Int
index (OrdCrossing (Left (ZoneEntry i _))) = i
index (OrdCrossing (Right (ZoneExit i _))) = i

instance Eq OrdCrossing where
    x == y = (index x) == (index y)

instance Ord OrdCrossing where
    compare x y = compare (index x) (index y)

instance Show OrdCrossing where
    show (OrdCrossing x) = show x

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

-- | Finds the first pair of points, one outside the zone and the next inside.
-- Searches the fixes in order.
entersZoneFwdSingle :: (Real a, Fractional a)
                    => SpanLatLng a
                    -> CrossingPredicate a ZoneEntry
entersZoneFwdSingle span z xs =
    case insideZone span z xs of
        Nothing -> []
        Just j ->
            case outsideZone span z . reverse $ take j xs of
                Just 0 -> [ZoneEntry (j - 1) j]
                _ -> []

-- | Finds the first pair of points, one outside the zone and the next inside.
-- Searches the fixes in reverse order. This avoids getting a false negative
-- for the entry test as can occur in some tasks where the zone we're checking
-- was used earlier in the task or is not separate to an earlier zone.
entersZoneRevSingle :: (Real a, Fractional a)
                    => SpanLatLng a
                    -> CrossingPredicate a ZoneEntry
entersZoneRevSingle span z xs =
    reflect <$> ys
    where
        ys = exitsZoneFwdSingle span z $ reverse xs
        nth = (length xs) - 1
        reflect (ZoneExit i j) = ZoneEntry (nth - j) (nth - i)

-- | Finds the first pair of points, one inside the zone and the next outside.
-- Searches the fixes in order.
exitsZoneFwdSingle :: (Real a, Fractional a)
                   => SpanLatLng a
                   -> CrossingPredicate a ZoneExit
exitsZoneFwdSingle span z xs =
    case outsideZone span z xs of
        Nothing -> []
        Just j ->
            case insideZone span z . reverse $ take j xs of
                Just 0 -> [ZoneExit (j - 1) j]
                _ -> []

exitsZoneRevSingle :: (Real a, Fractional a)
                   => SpanLatLng a
                   -> CrossingPredicate a ZoneExit
exitsZoneRevSingle span z xs =
    reflect <$> ys
    where
        ys = entersZoneFwdSingle span z $ reverse xs
        nth = (length xs) - 1
        reflect (ZoneEntry i j) = ZoneExit (nth - j) (nth - i)

entersZoneFwd :: (Real a, Fractional a)
              => SpanLatLng a
              -> CrossingPredicate a Crossing
entersZoneFwd span z xs = Left <$> (lefts $ entersZoneFwdSeq span z xs)

entersZoneRev :: (Real a, Fractional a)
              => SpanLatLng a
              -> CrossingPredicate a Crossing
entersZoneRev span z xs = Left <$> (lefts $ entersZoneRevSeq span z xs)

exitsZoneFwd :: (Real a, Fractional a)
             => SpanLatLng a
             -> CrossingPredicate a Crossing
exitsZoneFwd span z xs = Right <$> (rights $ exitsZoneFwdSeq span z xs)

exitsZoneRev :: (Real a, Fractional a)
             => SpanLatLng a
             -> CrossingPredicate a Crossing
exitsZoneRev span z xs = Right <$> (rights $ exitsZoneRevSeq span z xs)

-- | Find the sequence of @take _ [exit, entry.., exit, entry]@ going forward.
exitsZoneFwdSeq :: (Real a, Fractional a)
                => SpanLatLng a
                -> CrossingPredicate a Crossing
exitsZoneFwdSeq span z xs =
    case exitsZoneFwdSingle span z xs of
        [] ->
            []

        (hit@(ZoneExit i _) : _) ->
            Right hit
            : (entersZoneFwdSeq span z (drop i xs))

-- | Find the sequence of @take _ [entry, exit, .., entry, exit]@ but instead
-- of going forward through the track it works backwards.
entersZoneRevSeq :: (Real a, Fractional a)
                 => SpanLatLng a
                 -> CrossingPredicate a Crossing
entersZoneRevSeq span z xs =
    case entersZoneRevSingle span z xs of
        [] ->
            []

        (hit@(ZoneEntry i _) : _) ->
            (exitsZoneRevSeq span z (take i xs))
            ++ [Left hit]

-- | Find the sequence of @take _ [entry, exit, .., entry, exit]@ going forward.
entersZoneFwdSeq :: (Real a, Fractional a)
                 => SpanLatLng a
                 -> CrossingPredicate a Crossing
entersZoneFwdSeq span z xs =
    case entersZoneFwdSingle span z xs of
        [] ->
            []

        (hit@(ZoneEntry i _) : _) ->
            Left hit
            : (exitsZoneFwdSeq span z (drop i xs))

-- | Find the sequence of @take _ [entry, exit, .., entry, exit]@ but instead
-- of going forward through the track it works backwards.
exitsZoneRevSeq :: (Real a, Fractional a)
                 => SpanLatLng a
                 -> CrossingPredicate a Crossing
exitsZoneRevSeq span z xs =
    case exitsZoneRevSingle span z xs of
        [] ->
            []

        (hit@(ZoneExit i _) : _) ->
            (entersZoneRevSeq span z (take i xs))
            ++ [Right hit]

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
    -> [CrossingPredicate a Crossing]
pickCrossingPredicate span startIsExit Cmp.Task{speedSection, zones} =
    zipWith
        (\ i _ ->
            if (Just i) == end then entersRevCrossing span else
            -- NOTE: Any zone before the start is also treated as an
            -- exit cylinder if the start is an exit cylinder. This
            -- applies if the start cylinder wholly contains a prior
            -- zone or is separate to it.
            -- TODO: Consider overlapping zones before or at start.
            if i <= start && startIsExit then exitsFwdCrossing span
                                         else entersFwdCrossing span)
        [1 .. ]
        zones
    where
        (start, end) =
            case speedSection of
              Nothing -> (0, Nothing)
              Just (a, b) -> (a, Just b)

hitZone :: _ -> CrossingPredicate a Crossing
hitZone hit _ _ = [hit]

-- | Same as 'entersZoneRev' but returning a 'CrossingPredicate a Crossing'.
entersRevCrossing :: (Real a, Fractional a)
                  => SpanLatLng a
                  -> CrossingPredicate a Crossing
entersRevCrossing span = \z xs -> entersZoneRevSeq span z xs

-- | Same as 'exitsZoneFwd' but returning a 'CrossingPredicate a Crossing'.
exitsFwdCrossing :: (Real a, Fractional a)
                 => SpanLatLng a
                 -> CrossingPredicate a Crossing
exitsFwdCrossing span = \z xs -> exitsZoneFwdSeq span z xs

-- | Same as 'entersZoneFwd' but returning a 'CrossingPredicate a Crossing'.
entersFwdCrossing :: (Real a, Fractional a)
                  => SpanLatLng a
                  -> CrossingPredicate a Crossing
entersFwdCrossing span = \z xs -> entersZoneFwdSeq span z xs

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
        fsSpeed = (take n fsTicked) ++ (drop n $ slice speedSection fs)

        ys :: [Bool]
        ys = (not . null) <$> (tickedZones fsSpeed zsSpeed xs')

        numTicked = length $ takeWhile (== True) ys

        notTicked = drop numTicked zsSpeed

        zsNotTicked :: [Zone b]
        zsNotTicked = unTaskZone <$> notTicked

        xs' :: [TrackZone b]
        xs' = mkZone <$> xs

        cons :: a -> [Zone b]
        cons x = unTrackZone (mkZone x) : zsNotTicked
