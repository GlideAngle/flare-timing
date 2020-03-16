module Flight.Track.Lead2 (comp2Leading) where

import "newtype" Control.Newtype (Newtype(..))
import Data.List (sortOn)
import Data.UnitsOfMeasure ((+:), u, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (QTaskDistance)
import Flight.Comp (Pilot)
import Flight.Score
    ( LeadingArea(..), LeadingArea2Units, LeadingCoef(..), LeadingFraction(..)
    , leadingFraction, area2toCoef, mk2Coef
    )
import Flight.Track.Time (LeadingAreas(..), taskToLeading, minLeadingCoef)
import Flight.Track.Lead

comp2Leading
    :: DiscardingLead LeadingArea2Units
    -> [Maybe (QTaskDistance Double [u| m |])]
    ->
        ( [Maybe (LeadingCoef (Quantity Double [u| 1 |]))]
        , [[(Pilot, TrackLead LeadingArea2Units)]]
        )
comp2Leading DiscardingLead{areas = ass} lsTask =
    (lcMins, lead)
    where
        ks :: [Quantity Rational [u| (km^2)*s |] -> Quantity Double [u| 1 |]]
        ks =
                [ maybe
                    (const [u| 1 |])
                    (mk2Coef . area2toCoef)
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
