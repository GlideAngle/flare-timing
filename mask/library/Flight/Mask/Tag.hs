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

import GHC.Exts (groupWith)
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.List (nub, elemIndex)
import Control.Lens ((^?), element)

import qualified Flight.Kml as Kml
    (Fix, MarkedFixes(..), FixMark(..), Seconds(..))
import Flight.Track.Cross (Fix(..), ZoneCross(..))
import qualified Flight.Comp as Cmp (Task(..))
import Flight.TrackLog as Log (IxTask(..))
import Flight.Units ()
import Flight.Mask.Internal
    ( ZoneHit(..)
    , slice
    , exitsZone
    , entersZone
    , fixToPoint
    , zoneToCylinder
    , isStartExit
    , pickCrossingPredicate
    , fixFromFix
    , tickedZones
    )

-- | A masking produces a value from a task and tracklog fixes.
type SigMasking a = [Cmp.Task] -> Log.IxTask -> Kml.MarkedFixes -> a

newtype PilotTrackFixes = PilotTrackFixes Int deriving Show

countFixes :: Kml.MarkedFixes -> PilotTrackFixes
countFixes Kml.MarkedFixes{fixes} =
    PilotTrackFixes $ length fixes

-- | A pilot has launched if their tracklog has distinct fixes.
launched :: SigMasking Bool
launched _ _ Kml.MarkedFixes{fixes} =
    not . null . nub $ fixes

started :: SigMasking Bool
started tasks (IxTask i) Kml.MarkedFixes{fixes} =
    case tasks ^? element (i - 1) of
        Nothing -> False
        Just Cmp.Task{speedSection, zones} ->
            case slice speedSection zones of
                [] ->
                    False

                z : _ ->
                    let ez = exitsZone (zoneToCylinder z) (fixToPoint <$> fixes)
                    in case ez of
                         ZoneExit _ _ -> True
                         _ -> False

madeGoal :: SigMasking Bool
madeGoal tasks (IxTask i) Kml.MarkedFixes{fixes} =
    case tasks ^? element (i - 1) of
        Nothing -> False
        Just Cmp.Task{zones} ->
            case reverse zones of
                [] ->
                    False

                z : _ ->
                    let ez = entersZone (zoneToCylinder z) (fixToPoint <$> fixes)
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

madeZones :: [Cmp.Task]
          -> IxTask
          -> Kml.MarkedFixes
          -> [Maybe ZoneCross]
madeZones tasks (IxTask i) Kml.MarkedFixes{mark0, fixes} =
    case tasks ^? element (i - 1) of
        Nothing ->
            []

        Just task@Cmp.Task{zones} ->
            f <$> xs
            where
                fs = (\x -> pickCrossingPredicate (isStartExit x) x) task

                xs =
                    tickedZones
                        fs
                        (zoneToCylinder <$> zones)
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
groupByLeg :: [Cmp.Task]
           -> IxTask
           -> Kml.MarkedFixes
           -> [Kml.MarkedFixes]
groupByLeg tasks iTask mf@Kml.MarkedFixes{mark0, fixes} =
    (\zs -> mf{Kml.fixes = zs}) <$> ys
    where
        xs :: [Maybe Fix]
        xs = tagZones $ madeZones tasks iTask mf

        ts :: [Maybe UTCTime]
        ts = (fmap . fmap) time xs

        ys :: [[Kml.Fix]]
        ys =
            groupWith
                (\x -> elemIndex (Just $ fixToUtc mark0 x) ts)
                fixes
