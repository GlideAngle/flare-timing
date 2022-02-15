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
    , pilotArrivalLag
    , pilotEssTime
    , startGateTaken
    ) where

import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.UnitsOfMeasure (u, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Comp (StartEnd(..), StartEndMark, StartGate(..))
import "flight-gap-allot" Flight.Score (SpeedFraction(..), PilotTime(..))
import "flight-gap-math" Flight.Score (JumpedTheGun(..))
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
pilotTime _ StartEnd{unEnd = Nothing} = Nothing
pilotTime [] StartEnd{unStart, unEnd = Just end} =
    Just . PilotTime $ hrs unStart end
pilotTime gs StartEnd{unStart, unEnd = Just end} = do
    gs' <- nonEmpty gs
    let (_, StartGate g) = startGateTaken gs' unStart
    return . PilotTime $ hrs g end

secs :: UTCTime -> UTCTime -> Quantity Double [u| s |]
secs start end = fromRational' . MkQuantity . toRational $ diffUTCTime end start

hrs :: UTCTime -> UTCTime -> Quantity Double [u| h |]
hrs start end = convert $ secs start end

pilotArrivalLag :: UTCTime -> UTCTime -> Quantity Double [u| h |]
pilotArrivalLag = hrs

-- | The time the pilot ticked the end of the speed section.
pilotEssTime
    :: [StartGate]
    -> StartEndMark
    -> (Maybe (JumpedTheGun (Quantity Double [u| s |])), Maybe UTCTime)
pilotEssTime _ StartEnd{unEnd = Nothing} = (Nothing, Nothing)
pilotEssTime gs StartEnd{unStart, unEnd = e@(Just _)} =
    maybe
        (Nothing, e)
        (\gs' -> const e <$> startGateTaken gs' unStart)
        (nonEmpty gs)

-- | The start gate the pilot took.
startGateTaken
    :: NonEmpty StartGate
    -> UTCTime
    -- ^ The time the pilot crossed the start
    -> (Maybe (JumpedTheGun (Quantity Double [u| s |])), StartGate)
startGateTaken gs t =
    case gs of
        sg0@(StartGate g0) :| [] ->
            if | t < g0 -> (Just . JumpedTheGun $ secs t g0, sg0)
               | otherwise -> (Nothing, sg0)
        sg0@(StartGate g0) :| (sg1@(StartGate g1) : gs') ->
            if | t < g0 -> (Just . JumpedTheGun $ secs t g0, sg0)
               | t < g1 -> (Nothing, sg0)
               | otherwise -> startGateTaken (sg1 :| gs') t
