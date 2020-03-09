{-|
Module      : Flight.Track.Mask.Lead
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks masked with task control zones.
-}
module Flight.Track.Mask.Lead (MaskingLead(..), RaceTime(..), racing) where

import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime, diffUTCTime, addUTCTime)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Clip (FlyCut(..), FlyClipping(..))
import Flight.Distance (QTaskDistance)
import Flight.Comp
    (OpenClose(..), FirstLead(..), FirstStart(..), LastArrival(..), LastDown(..))
import Flight.Score
    ( Pilot(..)
    , LeadingCoef(..)
    , AreaToCoef(..)
    , EssTime(..)
    )
import Flight.Field (FieldOrdering(..))
import Flight.Units ()
import Flight.Track.Lead (TrackLead(..))
import Flight.Track.Mask.Cmp (cmp)

-- | For each task, the masking for leading for that task.
data MaskingLead =
    MaskingLead
        { raceTime :: [Maybe RaceTime]
        -- ^ For each task, the race times.
        , raceDistance :: [Maybe (QTaskDistance Double [u| m |])]
        -- ^ For each task, the distance of the speed section.
        , sumDistance :: [Maybe (QTaskDistance Double [u| m |])]
        -- ^ For each task, the sum of all distance flown over minimum distance.
        , leadAreaToCoef :: [Maybe (AreaToCoef (Quantity Rational [u| 1/((km^2)*s)|]))]
        -- ^ For each task, the scaling of leading area.
        , leadCoefMin :: [Maybe (LeadingCoef (Quantity Double [u| 1 |]))]
        -- ^ For each task, the minimum of all pilot's leading coefficient.
        , leadRank :: [[(Pilot, TrackLead)]]
        -- ^ For each task, the rank order of leading and leading fraction.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance FieldOrdering MaskingLead where fieldOrder _ = cmp

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
    clipToCut x@FlyCut{cut = Nothing} = x
    clipToCut x@FlyCut{cut = Just (_, t1), uncut = y@RaceTime{..}} =
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

