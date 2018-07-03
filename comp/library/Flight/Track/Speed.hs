{-|
Module      : Flight.Track.Speed
Copyright   : (c) Block Scope Limited 2018
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

The speed of a pilot's track.
-}
module Flight.Track.Speed
    ( TrackSpeed(..)
    , pilotTime
    , startGateTaken
    ) where

import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.UnitsOfMeasure (u, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Comp (StartEnd(..), StartEndMark, StartGate(..))
import Flight.Score (SpeedFraction(..), PilotTime(..))
import Flight.Units ()

-- ^ If arrived at goal then speed fraction.
data TrackSpeed =
    TrackSpeed
        { time :: PilotTime (Quantity Double [u| h |])
        , frac :: SpeedFraction
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | Pilot time over the speed section.
pilotTime
    :: [StartGate]
    -> StartEndMark
    -> Maybe (PilotTime (Quantity Double [u| h |]))
pilotTime _ StartEnd{unEnd = Nothing} =
    Nothing
pilotTime gs x@StartEnd{unStart, unEnd = Just end} =
    case gs of
        [] -> Just . PilotTime $ hrs unStart
        [StartGate g] -> Just . PilotTime $ hrs g
        StartGate g : gs' ->
            if unStart <= g
               then Just . PilotTime $ hrs g
               else pilotTime gs' x
    where
        secs :: UTCTime -> Quantity Double [u| s |]
        secs t = fromRational' . MkQuantity . toRational $ diffUTCTime end t

        hrs :: UTCTime -> Quantity Double [u| h |]
        hrs = convert . secs

-- | The start gate the pilot took.
startGateTaken
    :: [StartGate]
    -> UTCTime
    -- ^ The time the pilot crossed the start
    -> Maybe StartGate
startGateTaken gs t =
    case gs of
        [] -> Nothing
        [g] -> Just g
        sg@(StartGate g) : gs' ->
            if t <= g
               then Just sg
               else startGateTaken gs' t
