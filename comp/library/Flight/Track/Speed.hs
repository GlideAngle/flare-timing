{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Flight.Track.Speed
Copyright   : (c) Block Scope Limited 2018
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

The speed of a pilot's track.
-}
module Flight.Track.Speed
    ( TrackSpeed(..)
    , pilotTime
    ) where

import Data.Time.Clock (diffUTCTime)
import Data.UnitsOfMeasure (u, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Comp (StartEnd(..), StartEndMark)
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
pilotTime :: StartEndMark -> Maybe (PilotTime (Quantity Double [u| h |]))
pilotTime StartEnd{unEnd = Nothing} =
    Nothing
pilotTime StartEnd{unStart, unEnd = Just end} =
    Just . PilotTime $ hrs
    where
        secs :: Quantity Double [u| s |]
        secs = fromRational' . MkQuantity . toRational $ diffUTCTime end unStart

        hrs :: Quantity Double [u| h |]
        hrs = convert secs
