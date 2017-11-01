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
    , countFixes
    , madeZones
    , tagZones
    , launched
    , madeGoal
    , started
    , groupByLeg
    ) where

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
    ( ZoneHit(..)
    , TaskZone(..)
    , slice
    , exitsZone
    , entersZone
    , fixToPoint
    , isStartExit
    , pickCrossingPredicate
    , fixFromFix
    , tickedZones
    )
import qualified Flight.Zone.Raw as Raw (RawZone(..))

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

started :: Real a => (Raw.RawZone -> TaskZone a) -> SigMasking Bool
started zoneToCyl tasks (IxTask i) Kml.MarkedFixes{fixes} =
    case tasks ^? element (i - 1) of
        Nothing -> False
        Just Cmp.Task{speedSection, zones} ->
            case slice speedSection zones of
                [] ->
                    False

                z : _ ->
                    let ez = exitsZone (zoneToCyl z) (fixToPoint <$> fixes)
                    in case ez of
                         ZoneExit _ _ -> True
                         _ -> False

madeGoal :: Real a => (Raw.RawZone -> TaskZone a) -> SigMasking Bool
madeGoal zoneToCyl tasks (IxTask i) Kml.MarkedFixes{fixes} =
    case tasks ^? element (i - 1) of
        Nothing -> False
        Just Cmp.Task{zones} ->
            case reverse zones of
                [] ->
                    False

                z : _ ->
                    let ez = entersZone (zoneToCyl z) (fixToPoint <$> fixes)
                    in case ez of
                         ZoneEntry _ _ -> True
                         _ -> False

proof :: [Kml.Fix] -> UTCTime -> Int -> Int -> [Bool] -> Maybe ZoneCross
proof fixes mark0 i j bs = do
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

madeZones :: Real a
          => (Raw.RawZone -> TaskZone a)
          -> [Cmp.Task]
          -> IxTask
          -> Kml.MarkedFixes
          -> [Maybe ZoneCross]
madeZones zoneToCyl tasks (IxTask i) Kml.MarkedFixes{mark0, fixes} =
    case tasks ^? element (i - 1) of
        Nothing ->
            []

        Just task@Cmp.Task{zones} ->
            f <$> xs
            where
                fs = (\x -> pickCrossingPredicate (isStartExit zoneToCyl x) x) task

                xs =
                    tickedZones
                        fs
                        (zoneToCyl <$> zones)
                        (fixToPoint <$> fixes)

                f :: ZoneHit -> Maybe ZoneCross
                f ZoneMiss = Nothing
                f (ZoneExit m n) = proof fixes mark0 m n [True, False]
                f (ZoneEntry m n) = proof fixes mark0 m n [False, True]

fixToUtc :: UTCTime -> Kml.Fix -> UTCTime
fixToUtc mark0 x =
    let (Kml.Seconds secs) = Kml.mark x
    in fromInteger secs `addUTCTime` mark0

-- | Groups fixes by legs of the task.
groupByLeg :: Real a
           => (Raw.RawZone -> TaskZone a)
           -> [Cmp.Task]
           -> IxTask
           -> Kml.MarkedFixes
           -> [Kml.MarkedFixes]
groupByLeg zoneToCyl tasks iTask mf@Kml.MarkedFixes{mark0, fixes} =
    (\zs -> mf{Kml.fixes = zs}) <$> ys
    where
        xs :: [Maybe Fix]
        xs = tagZones $ madeZones zoneToCyl tasks iTask mf

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
                $ whenElt (\x -> elem (Just $ fixToUtc mark0 x) ts))
                fixes
