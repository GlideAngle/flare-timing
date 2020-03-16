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
    , comp2Leading
    , lwScalingDefault
    , cmpArea
    ) where

import "newtype" Control.Newtype (Newtype(..))
import Data.List (sortOn)
import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure ((+:), u, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Field (FieldOrdering(..))
import Flight.Distance (QTaskDistance)
import Flight.Comp (Pilot)
import Flight.Score
    ( LeadingArea(..), LeadingArea2Units, LeadingCoef(..), LeadingFraction(..)
    , LwScaling(..)
    , leadingFraction, areaToCoef, mkCoef
    )
import Flight.Track.Time (LeadingAreas(..), taskToLeading, minLeadingCoef)
import Flight.Zone.MkZones (Discipline(..))

-- | For each task, the discarding for leading for that task. Further fixes are
-- discarded and the leading areas collated.
data DiscardingLead =
    DiscardingLead
        { areasWithDistanceSquared :: [[(Pilot, LeadingAreas (LeadingArea LeadingArea2Units) (LeadingArea LeadingArea2Units))]]
        -- ^ For each task, the leading areas using distance squared.
        }
    deriving (Eq, Ord, Generic, ToJSON, FromJSON)

instance FieldOrdering DiscardingLead where fieldOrder _ = cmpArea compare

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

deriving instance (u ~ LeadingArea2Units) => FromJSON (TrackLead u)
deriving instance (u ~ LeadingArea2Units) => ToJSON (TrackLead u)

comp2Leading
    :: DiscardingLead
    -> [Maybe (QTaskDistance Double [u| m |])]
    ->
        ( [Maybe (LeadingCoef (Quantity Double [u| 1 |]))]
        , [[(Pilot, TrackLead LeadingArea2Units)]]
        )
comp2Leading DiscardingLead{areasWithDistanceSquared = ass} lsTask =
    (lcMins, lead)
    where
        ks :: [Quantity Rational [u| (km^2)*s |] -> Quantity Double [u| 1 |]]
        ks =
                [ maybe
                    (const [u| 1 |])
                    (mkCoef . areaToCoef)
                    l

                | l <- (fmap . fmap) taskToLeading lsTask
                ]

        css :: [[(Pilot, LeadingCoef (Quantity Double [u| 1 |]))]] =
                [ (fmap $ LeadingCoef . k . toRational' . unpack . sum2Areas) <$> as
                | k <- ks
                | as <- ass
                ]

        lcMins :: [Maybe (LeadingCoef (Quantity Double [u| 1 |]))]
        lcMins = minLeadingCoef <$> (fmap . fmap) snd css

        lead :: [[(Pilot, TrackLead LeadingArea2Units)]] =
                sortOn ((\TrackLead{coef = LeadingCoef c} -> c) . snd)
                <$>
                [
                    [
                        ( p
                        , TrackLead
                            { area = sum2Areas a
                            , coef = c
                            , frac =
                                maybe
                                    (LeadingFraction 0)
                                    (`leadingFraction` c)
                                    lcMin
                            }
                        )

                    | (_, a) <- as
                    | (p, c) <- cs
                    ]

                | lcMin <- lcMins
                | as <- ass
                | cs <- css
                ]

sum2Areas
    :: LeadingAreas (LeadingArea (Quantity Double [u| (km^2)*s |])) (LeadingArea (Quantity Double [u| (km^2)*s |]))
    -> (LeadingArea (Quantity Double [u| (km^2)*s |]))
sum2Areas LeadingAreas{areaFlown = LeadingArea af, areaAfterLanding = LeadingArea al} =
    LeadingArea $ af +: al

-- | The default explicit leading weight scaling for each discipline?
lwScalingDefault :: Discipline -> LwScaling
lwScalingDefault HangGliding = LwScaling 1
lwScalingDefault Paragliding = LwScaling 2
