{-|
Module      : Flight.Track.Distance
Copyright   : (c) Block Scope Limited 2018
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

The distance standing of a pilot's track in comparison to other pilots landing out.
-}
module Flight.Track.Distance
    ( TrackDistance(..)
    , AwardedDistance(..)
    , Nigh
    , Land
    ) where

import GHC.Generics (Generic)
import Data.Aeson
    ( ToJSON(..), FromJSON(..), Options(..)
    , defaultOptions, genericToJSON, genericParseJSON
    )
import Data.UnitsOfMeasure (u)

import Flight.Distance (QTaskDistance)
import Flight.Route (TrackLine(..))

type Nigh = TrackLine
type Land = QTaskDistance Double [u| m |]

-- | A distance awarded by the scorer for whatever reason. Commonly this is for
-- the pilot that does not submit their tracklog but it can also arise where
-- a scorer has needed to apply a penalty and so has had to freeze the distance
-- and nullify the tracklog.
newtype AwardedDistance = AwardedDistance {unawarded :: Land}
    deriving (Eq, Ord, Show, Generic)

instance ToJSON AwardedDistance where
    toJSON = genericToJSON defaultOptions{unwrapUnaryRecords = True}

instance FromJSON AwardedDistance where
    parseJSON = genericParseJSON defaultOptions{unwrapUnaryRecords = True}

data TrackDistance a =
    TrackDistance
        { togo :: Maybe a
        -- ^ The distance to goal.
        , made :: Maybe (QTaskDistance Double [u| m |])
        -- ^ The task distance minus the distance to goal.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
