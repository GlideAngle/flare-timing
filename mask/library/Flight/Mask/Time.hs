{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Mask.Time (timeFlown) where

import Prelude hiding (span)
import Data.Time.Clock (UTCTime, diffUTCTime)
import qualified Data.List as List (find)
import Data.Ratio ((%))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (SpanLatLng)
import Flight.Kml (Fix, Seconds(..), FixMark(..), MarkedFixes(..))
import Flight.Comp (Task(..), SpeedSection, OpenClose(..), StartGate(..))
import Flight.Score (PilotTime(..))
import Flight.Units ()
import Flight.Mask.Tag (madeGoal)
import Flight.Mask.Internal.Zone
    ( ZoneEntry(..), ZoneExit(..), Crossing, TaskZone(..), TrackZone(..)
    , slice, fixToPoint
    )
import Flight.Mask.Internal.Cross
    (CrossingPredicate, entersSeq, exitsSeq, isStartExit, crossingPredicates)
import qualified Flight.Zone.Raw as Raw (RawZone(..))

timeFlown :: (Real a, Fractional a)
          => SpanLatLng a
          -> (Raw.RawZone -> TaskZone a)
          -> Task
          -> MarkedFixes
          -> Maybe (PilotTime (Quantity Double [u| h |]))
timeFlown span zoneToCyl task@Task{speedSection, zones, zoneTimes, startGates} xs =
    if null zones || not atGoal then Nothing else
    flownDuration span speedSection fs cs zoneTimes startGates xs
    where
        fs =
            (\x ->
                let b = isStartExit span zoneToCyl x
                in crossingPredicates span b x) task

        cs = zoneToCyl <$> zones
        atGoal = madeGoal span zoneToCyl task xs

flownDuration :: (Real a, Fractional a)
              => SpanLatLng a
              -> SpeedSection
              -> [CrossingPredicate a Crossing]
              -> [TaskZone a]
              -> [OpenClose]
              -> [StartGate]
              -> MarkedFixes
              -> Maybe (PilotTime (Quantity Double [u| h |]))
flownDuration span speedSection fs zs os gs MarkedFixes{mark0, fixes}
    | null zs = Nothing
    | null fixes = Nothing
    | otherwise =
        durationViaZones span fixToPoint mark speedSection fs zs os gs mark0 fixes

durationViaZones :: (Real a, Fractional a)
                 => SpanLatLng a
                 -> (Fix -> TrackZone a)
                 -> (Fix -> Seconds)
                 -> SpeedSection
                 -> [CrossingPredicate a Crossing]
                 -> [TaskZone a]
                 -> [OpenClose]
                 -> [StartGate]
                 -> UTCTime
                 -> [Fix]
                 -> Maybe (PilotTime (Quantity Double [u| h |]))
durationViaZones span mkZone atTime speedSection _ zs os gs t0 xs =
    if null xs then Nothing else
    case (osSpeed, zsSpeed, reverse zsSpeed) of
        ([], _, _) -> Nothing
        (_, [], _) -> Nothing
        (_, _, []) -> Nothing
        (o0 : _, z0 : _, zN : _) -> duration o0 (z0, zN) xys
    where
        -- TODO: Don't assume end of speed section is goal.
        zsSpeed = slice speedSection zs
        osSpeed =
            -- NOTE: When there is only one open/close all zones
            -- have the same open/close.
            case os of
                [_] -> os
                _ -> slice speedSection os

        xys :: [(Fix, (TrackZone _, TrackZone _))]
        xys = (\(x, y) -> (y, (mkZone x, mkZone y))) <$> zip (drop 1 xs) xs

        -- TODO: Account for entry start zone.
        slots :: (TaskZone _, TaskZone _)
              -> [(Fix, (TrackZone _, TrackZone _))]
              -> (Maybe Seconds, Maybe Seconds)
        slots (z0, zN) xzs =
            (f <$> xz0, f <$> xzN)
            where
                exits' :: (Fix, (TrackZone _, TrackZone _)) -> Bool
                exits' (_, (zx, zy)) =
                    case exitsSeq span z0 [zx, zy] of
                        Right (ZoneExit _ _) : _ ->
                            True

                        _ ->
                            False

                enters' :: (Fix, (TrackZone _, TrackZone _)) -> Bool
                enters' (_, (zx, zy)) =
                    case entersSeq span zN [zx, zy] of
                        Left (ZoneEntry _ _) : _ ->
                            True

                        _ ->
                            False

                xz0 :: Maybe (Fix, (TrackZone _, TrackZone _))
                xz0 = List.find exits' xzs

                xzN :: Maybe (Fix, (TrackZone _, TrackZone _))
                xzN = List.find enters' xzs

                f = atTime . fst

        duration o z xzs =
            case slots z xzs of
                (Nothing, _) -> Nothing
                (_, Nothing) -> Nothing
                (Just s0, Just sN) ->
                    Just . PilotTime . MkQuantity . fromRational
                    $ (deltaFlying + deltaStart) % 1
                    where
                        gs' = reverse gs
                        laterStart (StartGate g) = g > t0

                        startTime =
                            case dropWhile laterStart gs' of
                                [] -> open o
                                (StartGate t : _) -> t

                        deltaStart :: Integer
                        deltaStart =
                            round $ t0 `diffUTCTime` startTime

                        (Seconds deltaFlying) = sN - s0
