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
    , TrackReach(..)
    , AwardedDistance(..)
    , Clamp(..)
    , Nigh
    , Effort
    , awardByFrac
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u, convert, unQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (TaskDistance(..), QTaskDistance)
import Flight.Route (TrackLine(..))
import "flight-gap-allot" Flight.Score (LinearFraction(..))

type Nigh = TrackLine
type Effort = QTaskDistance Double [u| m |]

-- | A distance awarded by the scorer for whatever reason. Commonly this is for
-- the pilot that does not submit their tracklog but it can also arise where
-- a scorer has needed to apply a penalty and so has had to freeze the distance
-- and nullify the tracklog.
data AwardedDistance =
    AwardedDistance
        { awardedMade :: Effort
        -- ^ The distance awarded by the scorer to the pilot as-is.
        , awardedTask :: Effort
        -- ^ The task distance as read.
        , awardedFrac :: Double
        -- ^ The fraction of the task distance awarded to the pilot.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TrackDistance a =
    TrackDistance
        { togo :: Maybe a
        -- ^ The distance to goal.
        , made :: Maybe (QTaskDistance Double [u| m |])
        -- ^ The task distance minus the distance to goal.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data TrackReach =
    TrackReach
        { reach :: QTaskDistance Double [u| m |]
        , frac :: LinearFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Whether to clamp the awarded fraction to be <= 1.
newtype Clamp = Clamp Bool deriving Eq

awardByFrac
    :: Clamp
    -> QTaskDistance Double [u| m |]
    -> AwardedDistance
    -> Quantity Double [u| km |]
awardByFrac c (TaskDistance td') AwardedDistance{awardedFrac = frac} =
    MkQuantity $ frac' * unQuantity (convert td' :: Quantity Double [u| km |])
    where
        frac' = if c == Clamp True then min 1 frac else frac
