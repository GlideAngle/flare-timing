{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Flight.Track.Time
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks aligned in time based on when the first pilot starts the speed section.
-}
module Flight.Track.Time (TimeRow(..)) where

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Flight.Pilot (Pilot(..))
import Flight.LatLng.Raw (RawLat, RawLng)

-- | For each task, the crossing for that task.
data TimeRow =
    TimeRow
        { time :: UTCTime
        , pilot :: Pilot
        , lat :: RawLat
        , lng :: RawLng
        , distance :: Double
        }
    deriving (Show, Generic)

instance ToJSON TimeRow
instance FromJSON TimeRow
