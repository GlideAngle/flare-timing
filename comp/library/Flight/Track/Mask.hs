{-|
Module      : Flight.Track.Mask
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks masked with task control zones.
-}
module Flight.Track.Mask
    ( Masking(..)
    , RaceTime(..)
    , racing
    ) where

import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime, diffUTCTime, addUTCTime)
import Data.String (IsString())
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Clip (FlyCut(..), FlyClipping(..))
import Flight.Distance (QTaskDistance)
import Flight.LatLng (QAlt)
import Flight.Comp
    (OpenClose(..), FirstLead(..), FirstStart(..), LastArrival(..), LastDown(..))
import Flight.Score
    ( Pilot(..)
    , PilotsAtEss(..)
    , BestTime(..)
    , LeadingAreaScaling(..)
    , LeadingCoefficient(..)
    , EssTime(..)
    )
import Flight.Field (FieldOrdering(..))
import Flight.Units ()
import Flight.Track.Speed (TrackSpeed(..))
import Flight.Track.Arrival (TrackArrival(..))
import Flight.Track.Lead (TrackLead(..))
import Flight.Track.Distance (TrackDistance(..), Nigh, Land)

-- | For each task, the masking for that task.
data Masking =
    Masking
        { pilotsAtEss :: [PilotsAtEss]
        -- ^ For each task, the number of pilots at goal.
        , raceTime :: [Maybe RaceTime]
        -- ^ For each task, the time of the last pilot crossing goal.
        , ssBestTime :: [Maybe (BestTime (Quantity Double [u| h |]))]
        -- ^ For each task, the best time ignoring start gates.
        , gsBestTime :: [Maybe (BestTime (Quantity Double [u| h |]))]
        -- ^ For each task, the best time from the start gate taken.
        , taskDistance :: [Maybe (QTaskDistance Double [u| m |])]
        -- ^ For each task, the task distance.
        , taskSpeedDistance :: [Maybe (QTaskDistance Double [u| m |])]
        -- ^ For each task, the speed section subset of the task distance.
        , bestDistance :: [Maybe (QTaskDistance Double [u| m |])]
        -- ^ For each task, the best distance made.
        , sumDistance :: [Maybe (QTaskDistance Double [u| m |])]
        -- ^ For each task, the sum of all distance flown over minimum distance.
        , leadScaling :: [Maybe LeadingAreaScaling]
        -- ^ For each task, the scaling of leading area.
        , leadCoefMin :: [Maybe LeadingCoefficient]
        -- ^ For each task, the minimum of all pilot's leading coefficient.
        , lead :: [[(Pilot, TrackLead)]]
        -- ^ For each task, the rank order of leading and leading fraction.
        , arrival :: [[(Pilot, TrackArrival)]]
        -- ^ For each task, the rank order of arrival at goal and arrival fraction.
        , ssSpeed :: [[(Pilot, TrackSpeed)]]
        -- ^ For each task, for each pilot making goal, their time for the
        -- speed section and speed fraction, ignoring any start gates.
        , gsSpeed :: [[(Pilot, TrackSpeed)]]
        -- ^ For each task, for each pilot making goal, their time for the
        -- speed section and speed fraction, taking into account start gates.
        , nigh :: [[(Pilot, TrackDistance Nigh)]]
        -- ^ For each task, the best distance of each pilot landing out.
        , land :: [[(Pilot, TrackDistance Land)]]
        -- ^ For each task, the distance of the landing spot for each pilot
        -- landing out.
        , altStopped :: [[(Pilot, QAlt Double [u| m |])]]
        -- ^ For each task, the altitude of each pilot at the score back time.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | The racing time for the speed section is required for leading points.
data RaceTime =
    RaceTime
        { openTask :: UTCTime
        -- ^ The time of first allowed crossing of the start of the speed section.
        , closeTask :: UTCTime
        -- ^ The time of last allowed crossing of the end of the speed section.
        , firstLead :: Maybe FirstLead
        , firstStart :: Maybe FirstStart
        , lastArrival :: Maybe LastArrival
        , leadArrival :: Maybe EssTime
        -- ^ When the last pilot arrives at goal, seconds from the time of first lead.
        , leadAllDown :: Maybe EssTime
        -- ^ When the last pilot lands, seconds from the time of first lead.
        , leadClose :: Maybe EssTime
        -- ^ When the task closes, seconds from the time of first lead.
        , tickClose :: Maybe EssTime
        -- ^ When the task closes, seconds from the time of first race start.
        , openClose :: EssTime
        -- ^ Seconds from open to close
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance FlyClipping UTCTime RaceTime where
    clipIndices _ = []
    clipToFlown x@FlyCut{cut = Nothing} = x
    clipToFlown x@FlyCut{cut = Just (_, t1), uncut = y@RaceTime{..}} =
        x{uncut = fromMaybe y uc}
        where
            oc =
                OpenClose
                    { open = openTask
                    , close = min t1 closeTask
                    }

            -- TODO: Review whether there is not a better and more explicit
            -- way to cut short the task deadline when calculating leading
            -- area.
            lastArrival' = Just . LastArrival $
                    maybe (close oc) (\(LastArrival t) -> min t1 t) lastArrival

            lastDown = LastDown <$> do
                FirstLead lead <- firstLead
                EssTime down <- leadAllDown 
                let secs = fromIntegral (round (fromRational down :: Double) :: Integer)
                return $ secs `addUTCTime` lead

            uc = racing (Just oc) firstLead firstStart lastArrival' lastDown

racing
    :: Maybe OpenClose
    -> Maybe FirstLead
    -> Maybe FirstStart
    -> Maybe LastArrival
    -> Maybe LastDown
    -> Maybe RaceTime
racing oc firstLead firstStart lastArrival lastDown = do
    OpenClose{open, close} <- oc
    return
        RaceTime
            { openTask = open
            , closeTask = close
            , firstLead = firstLead
            , firstStart = firstStart
            , lastArrival = lastArrival

            , leadArrival = EssTime . toRational <$> do
                FirstLead lead <- firstLead
                LastArrival end <- lastArrival
                return $ end `diffUTCTime` lead

            , leadAllDown = EssTime . toRational <$> do
                FirstLead lead <- firstLead
                LastDown down <- lastDown
                return $ down `diffUTCTime` lead

            , leadClose = EssTime . toRational <$> do
                FirstLead lead <- firstLead
                return $ close `diffUTCTime` lead

            , tickClose = EssTime . toRational <$> do
                FirstStart start <- firstStart
                return $ close `diffUTCTime` start

            , openClose = EssTime . toRational $
                close `diffUTCTime` open 
            }

instance FieldOrdering Masking where
    fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        -- TODO: first start time & last goal time & launched
        ("openTask", _) -> LT

        ("closeTask", "openTask") -> GT
        ("closeTask", _) -> LT

        ("firstStart", "openTask") -> GT
        ("firstStart", "closeTask") -> GT
        ("firstStart", _) -> LT

        ("lastArrival", "openTask") -> GT
        ("lastArrival", "closeTask") -> GT
        ("lastArrival", "firstStart") -> GT
        ("lastArrival", _) -> LT

        ("tickArrival", "openTask") -> GT
        ("tickArrival", "closeTask") -> GT
        ("tickArrival", "firstStart") -> GT
        ("tickArrival", "lastArrival") -> GT
        ("tickArrival", _) -> LT

        ("tickRace", "openTask") -> GT
        ("tickRace", "closeTask") -> GT
        ("tickRace", "firstStart") -> GT
        ("tickRace", "lastArrival") -> GT
        ("tickRace", "tickArrival") -> GT
        ("tickRace", _) -> LT

        ("tickTask", _) -> GT

        ("pilotsAtEss", _) -> LT

        ("raceTime", "pilotsAtEss") -> GT
        ("raceTime", _) -> LT

        ("best", _) -> LT
        ("last", _) -> GT

        ("ssBestTime", "pilotsAtEss") -> GT
        ("ssBestTime", "raceTime") -> GT
        ("ssBestTime", _) -> LT

        ("gsBestTime", "pilotsAtEss") -> GT
        ("gsBestTime", "raceTime") -> GT
        ("gsBestTime", "ssBestTime") -> GT
        ("gsBestTime", _) -> LT

        ("taskDistance", "pilotsAtEss") -> GT
        ("taskDistance", "raceTime") -> GT
        ("taskDistance", "ssBestTime") -> GT
        ("taskDistance", "gsBestTime") -> GT
        ("taskDistance", _) -> LT

        ("taskSpeedDistance", "pilotsAtEss") -> GT
        ("taskSpeedDistance", "raceTime") -> GT
        ("taskSpeedDistance", "ssBestTime") -> GT
        ("taskSpeedDistance", "gsBestTime") -> GT
        ("taskSpeedDistance", "taskDistance") -> GT
        ("taskSpeedDistance", _) -> LT

        ("bestDistance", "pilotsAtEss") -> GT
        ("bestDistance", "raceTime") -> GT
        ("bestDistance", "ssBestTime") -> GT
        ("bestDistance", "gsBestTime") -> GT
        ("bestDistance", "taskDistance") -> GT
        ("bestDistance", "taskSpeedDistance") -> GT
        ("bestDistance", _) -> LT

        ("sumDistance", "pilotsAtEss") -> GT
        ("sumDistance", "raceTime") -> GT
        ("sumDistance", "ssBestTime") -> GT
        ("sumDistance", "gsBestTime") -> GT
        ("sumDistance", "taskDistance") -> GT
        ("sumDistance", "taskSpeedDistance") -> GT
        ("sumDistance", "bestDistance") -> GT
        ("sumDistance", _) -> LT

        ("leadScaling", "pilotsAtEss") -> GT
        ("leadScaling", "raceTime") -> GT
        ("leadScaling", "ssBestTime") -> GT
        ("leadScaling", "gsBestTime") -> GT
        ("leadScaling", "taskDistance") -> GT
        ("leadScaling", "taskSpeedDistance") -> GT
        ("leadScaling", "bestDistance") -> GT
        ("leadScaling", "sumDistance") -> GT
        ("leadScaling", _) -> LT

        ("leadCoefMin", "pilotsAtEss") -> GT
        ("leadCoefMin", "raceTime") -> GT
        ("leadCoefMin", "ssBestTime") -> GT
        ("leadCoefMin", "gsBestTime") -> GT
        ("leadCoefMin", "taskDistance") -> GT
        ("leadCoefMin", "taskSpeedDistance") -> GT
        ("leadCoefMin", "bestDistance") -> GT
        ("leadCoefMin", "sumDistance") -> GT
        ("leadCoefMin", "leadScaling") -> GT
        ("leadCoefMin", _) -> LT

        ("lead", "pilotsAtEss") -> GT
        ("lead", "raceTime") -> GT
        ("lead", "ssBestTime") -> GT
        ("lead", "gsBestTime") -> GT
        ("lead", "taskDistance") -> GT
        ("lead", "bestDistance") -> GT
        ("lead", "sumDistance") -> GT
        ("lead", "leadScaling") -> GT
        ("lead", "leadCoefMin") -> GT
        ("lead", _) -> LT

        ("arrival", "pilotsAtEss") -> GT
        ("arrival", "raceTime") -> GT
        ("arrival", "ssBestTime") -> GT
        ("arrival", "gsBestTime") -> GT
        ("arrival", "taskDistance") -> GT
        ("arrival", "bestDistance") -> GT
        ("arrival", "sumDistance") -> GT
        ("arrival", "leadScaling") -> GT
        ("arrival", "leadCoefMin") -> GT
        ("arrival", "lead") -> GT
        ("arrival", _) -> LT

        ("ssSpeed", "pilotsAtEss") -> GT
        ("ssSpeed", "raceTime") -> GT
        ("ssSpeed", "ssBestTime") -> GT
        ("ssSpeed", "gsBestTime") -> GT
        ("ssSpeed", "taskDistance") -> GT
        ("ssSpeed", "bestDistance") -> GT
        ("ssSpeed", "sumDistance") -> GT
        ("ssSpeed", "leadScaling") -> GT
        ("ssSpeed", "leadCoefMin") -> GT
        ("ssSpeed", "lead") -> GT
        ("ssSpeed", "arrival") -> GT
        ("ssSpeed", _) -> LT

        ("gsSpeed", "pilotsAtEss") -> GT
        ("gsSpeed", "raceTime") -> GT
        ("gsSpeed", "ssBestTime") -> GT
        ("gsSpeed", "gsBestTime") -> GT
        ("gsSpeed", "taskDistance") -> GT
        ("gsSpeed", "bestDistance") -> GT
        ("gsSpeed", "sumDistance") -> GT
        ("gsSpeed", "leadScaling") -> GT
        ("gsSpeed", "leadCoefMin") -> GT
        ("gsSpeed", "lead") -> GT
        ("gsSpeed", "arrival") -> GT
        ("gsSpeed", "ssSpeed") -> GT
        ("gsSpeed", _) -> LT

        ("nigh", "pilotsAtEss") -> GT
        ("nigh", "raceTime") -> GT
        ("nigh", "ssBestTime") -> GT
        ("nigh", "gsBestTime") -> GT
        ("nigh", "taskDistance") -> GT
        ("nigh", "bestDistance") -> GT
        ("nigh", "sumDistance") -> GT
        ("nigh", "leadScaling") -> GT
        ("nigh", "leadCoefMin") -> GT
        ("nigh", "lead") -> GT
        ("nigh", "arrival") -> GT
        ("nigh", "ssSpeed") -> GT
        ("nigh", "gsSpeed") -> GT
        ("nigh", _) -> LT

        ("land", "altStopped") -> LT
        ("land", _) -> GT

        ("altStopped", _) -> GT

        ("coef", _) -> LT
        ("time", _) -> LT
        ("rank", _) -> LT
        ("frac", _) -> GT

        ("madeGoal", _) -> LT
        ("arrivalRank", "madeGoal") -> GT
        ("arrivalRank", _) -> LT
        ("timeToGoal", "madeGoal") -> GT
        ("timeToGoal", "arrivalRank") -> GT
        ("timeToGoal", _) -> LT

        ("togo", _) -> LT
        ("made", _) -> GT

        _ -> compare a b
