{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flight.Lookup
    ( flyingTimeRange
    , arrivalRank
    , pilotTime
    , ticked
    , compRoutes
    , compRaceTimes
    ) where

import Data.Time.Clock (UTCTime)
import Data.Maybe (fromMaybe)
import Control.Monad (join)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (TaskDistance(..))
import Flight.Kml (MarkedFixes(..))
import Flight.Comp
    ( IxTask(..)
    , Task(..)
    , Pilot(..)
    , RouteLookup(..)
    , SpeedSection
    , FlyingSection
    , StartEndMark
    , FirstStart(..)
    , FirstLead(..)
    , LastArrival(..)
    , StartEnd(..)
    )
import qualified Flight.Comp as Cmp (openClose)
import Flight.Score (PositionAtEss(..), PilotTime(..))
import Flight.Track.Mask (RaceTime(..), racing)
import Flight.Track.Cross (TrackFlyingSection(..))
import Flight.Mask (ZoneIdx, RaceSections(..))
import qualified Flight.Track.Speed as Speed (pilotTime)
import Flight.Lookup.Cross (FlyingLookup(..))
import Flight.Lookup.Tag
    (ArrivalRankLookup(..), TaskTimeLookup(..), TimeLookup(..), TickLookup(..))

flyingTimeRange :: FlyingLookup -> UTCTime -> IxTask -> Pilot -> FlyingSection UTCTime
flyingTimeRange (FlyingLookup get) mark0 iTask p =
    fromMaybe (Just (mark0, mark0))
    $ join (fmap flyingTimes . (\f -> f iTask p) <$> get)

arrivalRank
    :: ArrivalRankLookup
    -> MarkedFixes
    -> IxTask
    -> SpeedSection
    -> Pilot
    -> Maybe PositionAtEss
arrivalRank (ArrivalRankLookup get) mf iTask speedSection p =
    PositionAtEss . toInteger
    <$> join ((\f -> f iTask speedSection p mf) <$> get)

pilotTime
    :: TimeLookup
    -> MarkedFixes
    -> IxTask
    -> SpeedSection
    -> Pilot
    -> Maybe (PilotTime (Quantity Double [u| h |]))
pilotTime (TimeLookup get) mf iTask speedSection p=
    join
    $ Speed.pilotTime
    <$> join ((\f -> f iTask speedSection p mf) <$> get)

ticked
    :: TickLookup
    -> MarkedFixes
    -> IxTask
    -> SpeedSection
    -> Pilot
    -> RaceSections ZoneIdx
ticked (TickLookup get) mf iTask speedSection p =
    fromMaybe (RaceSections [] [] [])
    $ join ((\f -> f iTask speedSection p mf) <$> get)

compRoutes :: RouteLookup -> [IxTask] -> [Maybe (TaskDistance Double)]
compRoutes (RouteLookup get) iTasks =
    (\i -> join ((\g -> g i) <$> get)) <$> iTasks

compTimes :: TaskTimeLookup -> [IxTask] -> [Task] -> [Maybe StartEndMark]
compTimes (TaskTimeLookup get) iTasks tasks =
    join <$>
    [ ($ s) . ($ i) <$> get
    | i <- iTasks
    | s <- speedSection <$> tasks
    ]


compRaceTimes :: TaskTimeLookup -> [IxTask] -> [Task] -> [Maybe RaceTime]
compRaceTimes getTaskTime iTasks tasks =
    [ racing (Cmp.openClose ss zt) fl fs la
    | ss <- speedSection <$> tasks
    | zt <- zoneTimes <$> tasks
    | fl <- raceFirstLead
    | fs <- raceFirstStart
    | la <- raceLastArrival
    ]
    where
        raceStartEnd :: [Maybe StartEndMark] =
                compTimes getTaskTime iTasks tasks

        raceFirstLead :: [Maybe FirstLead] =
                (fmap . fmap) (FirstLead . unStart) raceStartEnd

        raceFirstStart :: [Maybe FirstStart] =
                (fmap . fmap) (FirstStart . unStart) raceStartEnd

        raceLastArrival :: [Maybe LastArrival] =
                join
                <$> (fmap . fmap) (fmap LastArrival . unEnd) raceStartEnd
