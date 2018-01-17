{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Flight.Track.Distance
Copyright   : (c) Block Scope Limited 2018
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

The distance standing of a pilot's track in comparison to other pilots landing out.
-}
module Flight.Track.Distance (TrackDistance(..)) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

data TrackDistance a =
    TrackDistance
        { togo :: Maybe a
        -- ^ The distance to goal.
        , made :: Maybe Double
        -- ^ The task distance minus the distance to goal.
        }
    deriving (Eq, Ord, Show, Generic)

instance (ToJSON a) => ToJSON (TrackDistance a)
instance (FromJSON a) => FromJSON (TrackDistance a)
