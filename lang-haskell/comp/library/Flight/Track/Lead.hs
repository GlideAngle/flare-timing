{-|
Module      : Flight.Track.Lead
Copyright   : (c) Block Scope Limited 2018
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

The lead standing of a pilot's track in comparison to other pilots.
-}
module Flight.Track.Lead
    ( TrackLead(..)
    , DiscardingLead(..)
    , lwScalingDefault
    , cmpArea
    ) where

import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (KnownUnit, Unpack, u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Field (FieldOrdering(..))
import Flight.Comp (Pilot)
import Flight.Score (LeadingArea(..), LeadingCoef(..), LeadingFraction(..), LwScaling(..))
import Flight.Track.Time (LeadingAreas(..))
import Flight.Zone.MkZones (Discipline(..))

-- | For each task, the discarding for leading for that task. Further fixes are
-- discarded and the leading areas collated.
newtype DiscardingLead u =
    DiscardingLead
        { areas :: [[(Pilot, LeadingAreas (LeadingArea u) (LeadingArea u))]]
        }
    deriving (Eq, Ord, Generic)

instance FieldOrdering (DiscardingLead u) where
    fieldOrder _ = cmpArea compare

cmpArea :: (Ord a, IsString a) => (a -> a -> Ordering) -> a -> a -> Ordering
cmpArea f a b =
    case (a, b) of
        ("areaFlown", _) -> LT

        ("areaAfterLanding", "areaFlown") -> GT
        ("areaAfterLanding", _) -> LT

        ("areaBeforeStart", _) -> GT

        _ -> f a b

data TrackLead u =
    TrackLead
        { area :: LeadingArea u
        , coef :: LeadingCoef (Quantity Double [u| 1 |])
        , frac :: LeadingFraction
        }
    deriving (Eq, Ord, Generic)

deriving anyclass instance (KnownUnit (Unpack u), q ~ Quantity Double u, FromJSON (LeadingArea q)) => FromJSON (TrackLead q)
deriving anyclass instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToJSON (LeadingArea q)) => ToJSON (TrackLead q)
deriving anyclass instance (KnownUnit (Unpack u), q ~ Quantity Double u, FromJSON (LeadingArea q)) => FromJSON (DiscardingLead q)
deriving anyclass instance (KnownUnit (Unpack u), q ~ Quantity Double u, ToJSON (LeadingArea q)) => ToJSON (DiscardingLead q)

-- | The default explicit leading weight scaling for each discipline?
lwScalingDefault :: Discipline -> LwScaling
lwScalingDefault HangGliding = LwScaling 1
lwScalingDefault Paragliding = LwScaling 2
