{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Flight.Track.Lead
Copyright   : (c) Block Scope Limited 2018
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

The lead standing of a pilot's track in comparison to other pilots.
-}
module Flight.Track.Lead (TrackLead(..)) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Score (LeadingCoefficient(..), LeadingFraction(..))

data TrackLead =
    TrackLead
        { coef :: LeadingCoefficient
        , frac :: LeadingFraction
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
