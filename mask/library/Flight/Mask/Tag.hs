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

module Flight.Mask.Tag
    ( SigMasking
    , SelectedCrossings(..)
    , NomineeCrossings(..)
    , countFixes
    , madeZones
    , tagZones
    , launched
    , madeGoal
    , started
    , groupByLeg
    ) where

import Prelude hiding (span)
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.List (nub)
import Data.List.Split (split, whenElt, keepDelimsL)
import Control.Lens ((^?), element)

import qualified Flight.Kml as Kml
    (Fix, MarkedFixes(..), FixMark(..), Seconds(..))
import Flight.Track.Cross (Fix(..), ZoneCross(..))
import qualified Flight.Comp as Cmp (Task(..))
import Flight.TrackLog (IxTask(..))
import Flight.Units ()
import Flight.Mask.Internal
    ( ZoneEntry(..)
    , ZoneExit(..)
    , Crossing
    , TaskZone(..)
    , OrdCrossing(..)
    , slice
    , fixToPoint
    , isStartExit
    , crossingPredicates
    , crossingSelectors
    , fixFromFix
    , tickedZones
    , entersSeq
    , exitsSeq
    )
import qualified Flight.Zone.Raw as Raw (RawZone(..))
import Flight.Task (SpanLatLng)

-- | A masking produces a value from a task and tracklog fixes.
type SigMasking a = [Cmp.Task] -> IxTask -> Kml.MarkedFixes -> a

newtype PilotTrackFixes = PilotTrackFixes Int deriving Show

countFixes :: Kml.MarkedFixes -> PilotTrackFixes
countFixes Kml.MarkedFixes{fixes} =
    PilotTrackFixes $ length fixes

-- | A pilot has launched if their tracklog has distinct fixes.
launched :: SigMasking Bool
launched _ _ Kml.MarkedFixes{fixes} =
    not . null . nub $ fixes

started :: (Real a, Fractional a)
        => SpanLatLng a
        -> (Raw.RawZone -> TaskZone a)
        -> SigMasking Bool
started span zoneToCyl tasks (IxTask i) Kml.MarkedFixes{fixes} =
    case tasks ^? element (i - 1) of
        Nothing -> False
        Just Cmp.Task{speedSection, zones} ->
            case slice speedSection zones of
                [] ->
                    False

                z : _ ->
                    let ez = exitsSeq span (zoneToCyl z) (fixToPoint <$> fixes)
                    in case ez of
                         Right (ZoneExit _ _) : _ ->
                             True

                         _ ->
                             False

madeGoal :: (Real a, Fractional a)
         => SpanLatLng a
         -> (Raw.RawZone -> TaskZone a)
         -> SigMasking Bool
madeGoal span zoneToCyl tasks (IxTask i) Kml.MarkedFixes{fixes} =
    case tasks ^? element (i - 1) of
        Nothing -> False
        Just Cmp.Task{zones} ->
            case reverse zones of
                [] ->
                    False

                z : _ ->
                    let ez = entersSeq span (zoneToCyl z) (fixToPoint <$> fixes)
                    in case ez of
                         Left (ZoneEntry _ _) : _ ->
                             True

                         _ ->
                             False

-- | Prove from the fixes and mark that the crossing exits.
prove :: [Kml.Fix] -> UTCTime -> Int -> Int -> [Bool] -> Maybe ZoneCross
prove fixes mark0 i j bs = do
    fixM <- fixes ^? element i
    fixN <- fixes ^? element j
    let fs = fixFromFix mark0 <$> [fixM, fixN]
    return ZoneCross { crossingPair = fs
                     , inZone = bs
                     }

-- | Given two points on either side of a zone, what is the crossing tag.
crossingTag :: (Fix, Fix) -> (Bool, Bool) -> Maybe Fix

crossingTag (fixM, _) (True, False) =
    -- TODO: Interpolate between crossing points. For now I just take the point on
    -- the inside.
    Just fixM

crossingTag (_, fixN) (False, True) =
    Just fixN

crossingTag _ _ =
    Nothing

tagZones :: [Maybe ZoneCross] -> [Maybe Fix]
tagZones =
    fmap (>>= f)
    where
        f :: ZoneCross -> Maybe Fix
        f ZoneCross{crossingPair, inZone} =
            case (crossingPair, inZone) of
                ([x, y], [a, b]) -> crossingTag (x, y) (a, b)
                _ -> Nothing

newtype SelectedCrossings =
    SelectedCrossings { unSelectedCrossings :: [Maybe ZoneCross] }
    deriving Show

newtype NomineeCrossings =
    NomineeCrossings { unNomineeCrossings :: [[Maybe ZoneCross]] }
    deriving Show

madeZones :: (Real a, Fractional a)
          => SpanLatLng a
          -> (Raw.RawZone -> TaskZone a)
          -> [Cmp.Task]
          -> IxTask
          -> Kml.MarkedFixes
          -> (SelectedCrossings, NomineeCrossings)
madeZones span zoneToCyl tasks (IxTask i) Kml.MarkedFixes{mark0, fixes} =
    case tasks ^? element (i - 1) of
        Nothing ->
            (SelectedCrossings [], NomineeCrossings [])

        Just task@Cmp.Task{zones} ->
            (selected, nominees)
            where
                nominees = NomineeCrossings $ f <$> xs

                ys :: [[OrdCrossing]]
                ys = trimOrdLists ((fmap . fmap) OrdCrossing xs)

                ys' :: [[Crossing]]
                ys' = (fmap . fmap) unOrdCrossing ys

                selectors :: [[Crossing] -> Maybe Crossing]
                selectors =
                    (\x ->
                        let b = isStartExit span zoneToCyl x
                        in crossingSelectors b x) task

                prover = proveCrossing fixes mark0

                selected =
                    SelectedCrossings
                    $ zipWith (selectZoneCross prover) selectors ys'

                fs =
                    (\x ->
                        let b = isStartExit span zoneToCyl x
                        in crossingPredicates span b x) task

                xs =
                    tickedZones
                        fs
                        (zoneToCyl <$> zones)
                        (fixToPoint <$> fixes)

                f :: [Crossing] -> [Maybe ZoneCross]
                f [] = []

                f (Right (ZoneExit m n) : es) =
                    p : f es
                    where
                        p = prove fixes mark0 m n [True, False]

                f (Left (ZoneEntry m n) : es) =
                    p : f es
                    where
                        p = prove fixes mark0 m n [False, True]

selectZoneCross :: (Crossing -> Maybe ZoneCross)
                -> ([Crossing] -> Maybe Crossing)
                -> [Crossing]
                -> Maybe ZoneCross
selectZoneCross prover selectCrossing xs = do
    x <- selectCrossing xs
    prover x

-- | If I have three sorted lists xs, ys and zs, discard elements of ys that
-- are greater than the last element of zs, then again filter ys so that each
-- element is greater than the first element of xs.
--
-- The reason for doing the comparison between ys and zs first is that on
-- triangle courses,  the first zone is commonly not part of the speed section.
-- This zone may only have crossings made after goal, at the end of the day's
-- racing. These will have high indices and I want to discard them early on in
-- the trimming.
--
-- It is alright too to end up with a null first list of crossings. This will
-- happen in an aerotow comp when the pilot is towed up from outside the first
-- zone.
--
-- >>>
trimToOrder :: Ord a => [a] -> [a] -> [a] -> [a]

trimToOrder xs ys zs@(_ : _) =
    case xs' of
        [] -> ys'
        (x : _) -> filter (> x) ys'
    where
        (xs', ys') =
            case reverse zs of
                [] -> ([], [])
                (z : _) -> (filter (< z) xs, filter (< z) ys)

trimToOrder (x : _) ys _ = filter (> x) ys
trimToOrder _ ys _ = ys

-- | Removes elements of the list of lists so that each list only has elements
-- less than elements of subsequent lists and greater than previous lists.
--
-- >>>
-- > trimOrdLists [[25,4953,4955],[783,809,811,817,820,822,952,4812],[1810,1816],[3778,3781],[30,66,144,145,149,151,153,4950,4960,4965]]
--
-- [[25],[783,809,811,817,820,822,952],[1810,1816],[3778,3781],[4950,4960,4965]]
--
-- > trimOrdLists [[294,4714,4720,4724],[1367,4597],[2207,2209],[3914,3920],[300,568,570,572,573,4711]]
--
-- [[294],[1367],[2207,2209],[3914,3920],[4711]]
--
-- In another example a pilot only makes it around some of the course but
-- had flown through the goal cylinder before starting the speed section.
--
-- >>>
-- > trimOrdLists [[29],[276],[],[],[32,67,68,69,78]]
--
-- [[29],[276],[],[],[]]
--
-- > trimOrdLists [[4588,4592],[],[1714,1720],[3539,3546],[4584]]
--
-- [[4588,4592],[],[],[],[]]
--
-- > trimOrdLists [[4588,4592],[30,578,583,721,4400],[1714,1720],[3539,3546],[4584]]
--
-- [[],[30,578,583,721],[1714,1720],[3539,3546],[4584]]
--
-- > trimOrdLists [[294,4714,4720,4724],[1367,4597],[2207,2209],[3914,3920],[300,568,570,572,573,4711]]
--
-- [[294],[1367],[2207,2209],[3914,3920],[4711]]
trimOrdLists :: Ord a => [[a]] -> [[a]]
trimOrdLists ys =
    if ys == ys' then nonNullBlock ys else trimOrdLists ys'
    where
        xs = [] : ys
        zs = drop 1 ys ++ [[]]
        ys' = zipWith3 trimToOrder xs ys zs

-- | Going left to right, as soon as an empty list is encountered, all
-- subsequent lists are made null too.
prependNull :: Int -> [[a]] -> [[a]]
prependNull n xs =
    replicate n [] ++ xs

-- | Going left to right, as soon as an empty list is encountered, all
-- subsequent lists are made null too.
lockNullRight :: [[a]] -> [[a]]
lockNullRight xs =
    take (length xs) $ takeWhile (not . null) xs ++ repeat []

-- | Nulls are kept on the left, then a sequence of non-null lists. On the
-- first occurence of a null list, all further lists are replaced by null lists
-- on the right.
nonNullBlock :: [[a]] -> [[a]]
nonNullBlock xs =
    prependNull (length xs - length ys) ys
    where
        ys = lockNullRight $ dropWhile null xs

proveCrossing :: [Kml.Fix] -> UTCTime -> Crossing -> Maybe ZoneCross
proveCrossing fixes mark0 (Right (ZoneExit m n)) =
    prove fixes mark0 m n [True, False]

proveCrossing fixes mark0 (Left (ZoneEntry m n)) =
    prove fixes mark0 m n [False, True]

fixToUtc :: UTCTime -> Kml.Fix -> UTCTime
fixToUtc mark0 x =
    let (Kml.Seconds secs) = Kml.mark x
    in fromInteger secs `addUTCTime` mark0

-- | Groups fixes by legs of the task.
groupByLeg :: (Real a, Fractional a)
           => SpanLatLng a
           -> (Raw.RawZone -> TaskZone a)
           -> [Cmp.Task]
           -> IxTask
           -> Kml.MarkedFixes
           -> [Kml.MarkedFixes]
groupByLeg span zoneToCyl tasks iTask mf@Kml.MarkedFixes{mark0, fixes} =
    (\zs -> mf{Kml.fixes = zs}) <$> ys
    where
        xs :: [Maybe Fix]
        xs =
            tagZones . unSelectedCrossings . fst
            $ madeZones span zoneToCyl tasks iTask mf

        ts :: [Maybe UTCTime]
        ts = (fmap . fmap) time xs

        {- NOTE: A small ghci session showing splitting.
        let a = ['a' .. 'z']
        "abcdefghijklmnopqrstuvwxyz"

        let b = [Nothing, Just 'c', Just 't', Nothing]
        [Nothing,Just 'c',Just 't',Nothing]

        split (whenElt (\x -> elem (Just x) b)) a
        ["ab","c","defghijklmnopqrs","t","uvwxyz"]

        split (keepDelimsR $ whenElt (\x -> elem (Just x) b)) a
        ["abc","defghijklmnopqrst","uvwxyz"]

        split (keepDelimsL $ whenElt (\x -> elem (Just x) b)) a
        ["ab","cdefghijklmnopqrs","tuvwxyz"]
        -}
        ys :: [[Kml.Fix]]
        ys =
            split
                (keepDelimsL
                $ whenElt (\x -> (Just $ fixToUtc mark0 x) `elem` ts))
                fixes
