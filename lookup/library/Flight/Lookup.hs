module Flight.Lookup
    ( flyingTimeRange
    , scoredTimeRange
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

import Flight.Kml (MarkedFixes(..))
import Flight.Track.Cross (FlyingSection)
import Flight.Comp
    ( IxTask(..)
    , Task(..)
    , Pilot(..)
    , RoutesLookupTaskDistance(..)
    , TaskRouteDistance
    , SpeedSection
    , StartGate
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
import Flight.Track.Time (ZoneIdx)
import Flight.Mask (RaceSections(..))
import qualified Flight.Track.Speed as Speed (pilotTime)
import Flight.Lookup.Cross (FlyingLookup(..))
import Flight.Lookup.Tag
    (ArrivalRankLookup(..), TaskTimeLookup(..), TimeLookup(..), TickLookup(..))

flyingTimeRange :: FlyingLookup -> UTCTime -> IxTask -> Pilot -> FlyingSection UTCTime
flyingTimeRange (FlyingLookup get) mark0 iTask p =
    fromMaybe
        (Just (mark0, mark0))
        (fmap flyingTimes . (\f -> f iTask p) =<< get)

scoredTimeRange :: FlyingLookup -> UTCTime -> IxTask -> Pilot -> FlyingSection UTCTime
scoredTimeRange (FlyingLookup get) mark0 iTask p =
    fromMaybe
        (Just (mark0, mark0))
        (fmap scoredTimes . (\f -> f iTask p) =<< get)

arrivalRank
    :: ArrivalRankLookup
    -> MarkedFixes
    -> IxTask
    -> SpeedSection
    -> Pilot
    -> Maybe PositionAtEss
arrivalRank (ArrivalRankLookup get) mf iTask speedSection p =
    PositionAtEss . toInteger
    <$> ((\f -> f iTask speedSection p mf) =<< get)

pilotTime
    :: TimeLookup
    -> MarkedFixes
    -> IxTask
    -> [StartGate]
    -> SpeedSection
    -> Pilot
    -> Maybe (PilotTime (Quantity Double [u| h |]))
pilotTime (TimeLookup get) mf iTask startGates speedSection p =
    Speed.pilotTime startGates
    =<< ((\f -> f iTask speedSection p mf) =<< get)

ticked
    :: TickLookup
    -> MarkedFixes
    -> IxTask
    -> SpeedSection
    -> Pilot
    -> RaceSections ZoneIdx
ticked (TickLookup get) mf iTask speedSection p =
    fromMaybe
        (RaceSections [] [] [])
        ((\f -> f iTask speedSection p mf) =<< get)

compRoutes
    :: RoutesLookupTaskDistance
    -> [IxTask]
    -> [Maybe TaskRouteDistance]
compRoutes (RoutesLookupTaskDistance get) iTasks =
    (\i -> ((\g -> g i) =<< get)) <$> iTasks

compTimes :: TaskTimeLookup -> [IxTask] -> [Task k] -> [Maybe StartEndMark]
compTimes (TaskTimeLookup get) iTasks tasks =
    join <$>
    [ ($ s) . ($ i) <$> get
    | i <- iTasks
    | s <- speedSection <$> tasks
    ]


compRaceTimes :: TaskTimeLookup -> [IxTask] -> [Task k] -> [Maybe RaceTime]
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
