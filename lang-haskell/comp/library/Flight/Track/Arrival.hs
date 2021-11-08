{-|
Module      : Flight.Track.Arrival
Copyright   : (c) Block Scope Limited 2018
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

The arrival standing of a pilot's track in comparison to other pilots.
-}
module Flight.Track.Arrival
    ( ArrivalInputs
    , TrackArrival(..)
    , arrivalsByRank
    , arrivalsByTime
    ) where

import Data.Time.Clock (UTCTime)
import Data.Maybe (fromMaybe)
import Data.List (sortOn)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Track.Speed (pilotArrivalLag)
import Flight.Track.Place (rankByArrival)
import "flight-gap-allot" Flight.Score
    ( Pilot(..), ArrivalPlacing(..), ArrivalFraction(..), ArrivalLag(..)
    , PilotsAtEss(..)
    , arrivalRankFraction, arrivalTimeFraction
    )

-- ^ If arrived at goal then arrival rank and fraction.
data TrackArrival =
    TrackArrival
        { rank :: ArrivalPlacing
        , lag :: ArrivalLag (Quantity Double [u| h |])
        , frac :: ArrivalFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

type ArrivalInputs = [(Pilot, UTCTime)]

placeAndLag
    :: [(a, UTCTime)]
    -> (PilotsAtEss, [(a, (ArrivalPlacing, ArrivalLag (Quantity Double [u| h |])))])
placeAndLag ys = (pilots, ys') where
    n = toInteger $ length ys

    pilots :: PilotsAtEss
    pilots = PilotsAtEss n

    ts = snd <$> ys
    minT = minimum ts
    placings = rankByArrival ts
    place t = fromMaybe (ArrivalPlacing n) (lookup t placings)

    ys' =
        (\(p, t) ->
            let lag = ArrivalLag $ pilotArrivalLag minT t
            in (p, (place t, lag)))
        <$> ys

arrivalsByRank :: ArrivalInputs -> [(Pilot, TrackArrival)]
arrivalsByRank ys =
    sortOn (rank . snd) $ (fmap . fmap) f ys'
    where
        (pilots, ys') = placeAndLag ys

        f (position, tm) =
            TrackArrival
                { rank = position
                , lag = tm
                , frac = arrivalRankFraction pilots position
                }

arrivalsByTime :: ArrivalInputs -> [(Pilot, TrackArrival)]
arrivalsByTime ys =
    sortOn (rank . snd) $ (fmap . fmap) f ys'
    where
        (pilots, ys') = placeAndLag ys

        f (pEss, tm) =
            TrackArrival
                { rank = pEss
                , lag = tm
                , frac = arrivalTimeFraction pilots tm
                }
