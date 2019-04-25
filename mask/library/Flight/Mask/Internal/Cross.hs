module Flight.Mask.Internal.Cross
    ( CrossingPredicate
    , isStartExit
    , crossingPredicates
    , crossingSelectors
    , tickedZones
    , entersSeq
    , exitsSeq
    , reindex
    ) where

import Prelude hiding (span)
import Data.Maybe (listToMaybe)
import Data.List (nub, sort, findIndex)
import Control.Lens ((^?), element)

import Flight.LatLng (AzimuthFwd)
import Flight.Distance (SpanLatLng)
import Flight.Comp (Task(..), Zones(..))
import Flight.Track.Time (ZoneIdx(..))
import Flight.Units ()
import Flight.Mask.Internal.Zone
    ( TaskZone(..)
    , TrackZone(..)
    , ZoneEntry(..)
    , ZoneExit(..)
    , Crossing
    , OrdCrossing(..)
    )
import Flight.Earth.Sphere.Separated (separatedZones)

-- | A function that tests whether a flight track, represented as a series of point
-- zones crosses a zone.
type CrossingPredicate a b
    = TaskZone a
    -- ^ The task control zone.
    -> [TrackZone a]
    -- ^ The flight track represented as a series of point zones.
    -> [b]

insideZone
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> TaskZone a
    -> [TrackZone a]
    -> Maybe ZoneIdx
insideZone az span (TaskZone z) =
    fmap ZoneIdx
    . findIndex (\(TrackZone x) -> not $ separatedZones az span [x, z])

outsideZone
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> TaskZone a
    -> [TrackZone a]
    -> Maybe ZoneIdx
outsideZone az span (TaskZone z) =
    fmap ZoneIdx
    . findIndex (\(TrackZone x) -> separatedZones az span [x, z])

zoneSingle
    :: (az -> span -> zone -> [x] -> Maybe ZoneIdx)
    -> (az -> span -> zone -> [x] -> Maybe ZoneIdx)
    -> (ZoneIdx -> ZoneIdx -> crossing)
    -> az
    -> span
    -> zone
    -> [x]
    -> [crossing]
zoneSingle f g ctor az span z xs =
    case g az span z xs of
        Nothing -> []
        Just j@(ZoneIdx j') ->
            case f az span z . reverse $ ys of
                Just 0 -> [ctor (j - 1) j]
                _ -> []
            where
                ys = take j' xs

-- | Finds the first pair of points, one outside the zone and the next inside.
-- Searches the fixes in order.
entersSingle
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> CrossingPredicate a ZoneEntry
entersSingle =
    zoneSingle outsideZone insideZone ZoneEntry

-- | Finds the first pair of points, one inside the zone and the next outside.
-- Searches the fixes in order.
exitsSingle
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> CrossingPredicate a ZoneExit
exitsSingle  =
    zoneSingle insideZone outsideZone ZoneExit

reindex
    :: ZoneIdx -- ^ The length of the track, the number of fixes
    -> Either ZoneEntry ZoneExit
    -> Either ZoneEntry ZoneExit
reindex n (Right (ZoneExit i j)) =
    Right $ ZoneExit (i + n) (j + n)

reindex n (Left (ZoneEntry i j)) =
    Left $ ZoneEntry (i + n) (j + n)

crossSeq
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> CrossingPredicate a Crossing
crossSeq az span z xs =
    unOrdCrossing <$> (nub . sort $ enters ++ exits)
    where
        enters = OrdCrossing <$> entersSeq az span z xs
        exits = OrdCrossing <$> exitsSeq az span z xs

-- | Find the sequence of @take _ [entry, exit, .., entry, exit]@ going forward.
entersSeq
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> CrossingPredicate a Crossing
entersSeq az span z xs =
    case entersSingle az span z xs of
        [] ->
            []

        (hit@(ZoneEntry _ jIdx@(ZoneIdx j)) : _) ->
            Left hit : (reindex jIdx <$> exitsSeq az span z (drop j xs))

-- | Find the sequence of @take _ [exit, entry.., exit, entry]@ going forward.
exitsSeq
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> CrossingPredicate a Crossing
exitsSeq az span z xs =
    case exitsSingle az span z xs of
        [] ->
            []

        (hit@(ZoneExit _ jIdx@(ZoneIdx j)) : _) ->
            Right hit : (reindex jIdx <$> entersSeq az span z (drop j xs))

-- | A start zone is either entry or exit when all other zones are entry.
-- If I must fly into the start cylinder to reach the next turnpoint then
-- the start zone is entry otherwise it is exit. In one case the start cylinder
-- contains the next turnpoint and in the other the start cylinder is
-- completely separate from the next turnpoint.
isStartExit
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> (Zones -> [TaskZone a])
    -> Task k
    -> Bool
isStartExit az span fromZones Task{speedSection, zones} =
    case speedSection of
        Nothing ->
            False

        Just (i, _) ->
            let zs = fromZones zones in
            case (zs ^? element (i - 1), zs ^? element i) of
                (Just start, Just tp1) ->
                    separatedZones az span
                    $ unTaskZone
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
crossingPredicates
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> Bool -- ^ Is the start an exit cylinder?
    -> Task k
    -> [CrossingPredicate a Crossing]
crossingPredicates az span _ Task{zones} =
    const (crossSeq az span) <$> raw zones

-- | If the zone is an exit, then take the last crossing otherwise take the
-- first crossing.
crossingSelectors
    :: Bool -- ^ Is the start an exit cylinder?
    -> Task k
    -> [[a] -> Maybe a] -- ^ A crossing selector for each zone.
crossingSelectors startIsExit Task{speedSection, zones} =
    zipWith
        (\ i _ ->
            if i == start && startIsExit then selectLast
                                         else selectFirst)
        [1 .. ]
        (raw zones)
    where
        start =
            maybe 0 fst speedSection

selectFirst :: [a] -> Maybe a
selectFirst = listToMaybe . take 1

selectLast :: [a] -> Maybe a
selectLast xs = listToMaybe . take 1 $ reverse xs

tickedZones
    :: [CrossingPredicate a b]
    -> [TaskZone a] -- ^ The control zones of the task.
    -> [TrackZone a] -- ^ The flown track.
    -> [[b]]
tickedZones fs zones xs =
    zipWith (\f z -> f z xs) fs zones
