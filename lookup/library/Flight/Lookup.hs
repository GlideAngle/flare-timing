{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Flight.Lookup
    ( flyingTimeRange
    , arrivalRank
    , pilotTime
    , ticked
    ) where

import Data.Time.Clock (UTCTime)
import Data.Maybe (fromMaybe)
import Control.Monad (join)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Kml (MarkedFixes(..))
import Flight.Comp (IxTask(..), Pilot(..), SpeedSection, FlyingSection)
import Flight.Score
import Flight.Track.Cross (TrackFlyingSection(..))
import Flight.Mask (ZoneIdx, RaceSections(..))
import qualified Flight.Track.Speed as Speed (pilotTime)
import Flight.Lookup.Cross (FlyingLookup(..))
import Flight.Lookup.Tag (ArrivalRankLookup(..), TimeLookup(..), TickLookup(..))

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
