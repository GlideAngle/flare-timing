module Flight.Track.Lead1 (comp1Leading) where

import "newtype" Control.Newtype (Newtype(..))
import Data.List (sortOn)
import Data.UnitsOfMeasure ((+:), u, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (QTaskDistance)
import Flight.Comp (Pilot)
import Flight.Score
    ( LeadingArea(..), LeadingArea1Units, LeadingCoef(..), LeadingFraction(..)
    , leadingFraction, area1toCoef, mk1Coef
    )
import Flight.Track.Time (LeadingAreas(..), taskToLeading, minLeadingCoef)
import Flight.Track.Lead

comp1Leading
    :: DiscardingLead LeadingArea1Units
    -> [Maybe (QTaskDistance Double [u| m |])]
    ->
        ( [Maybe (LeadingCoef (Quantity Double [u| 1 |]))]
        , [[(Pilot, TrackLead LeadingArea1Units)]]
        )
comp1Leading DiscardingLead{areas = ass} lsTask =
    (lcMins, lead)
    where
        ks :: [Quantity Rational [u| km*s |] -> Quantity Double [u| 1 |]]
        ks =
                [ maybe
                    (const [u| 1 |])
                    (mk1Coef . area1toCoef)
                    l

                | l <- (fmap . fmap) taskToLeading lsTask
                ]

        css :: [[(Pilot, LeadingCoef (Quantity Double [u| 1 |]))]] =
                [ (fmap $ LeadingCoef . k . toRational' . unpack . sum1Areas) <$> as
                | k <- ks
                | as <- ass
                ]

        lcMins :: [Maybe (LeadingCoef (Quantity Double [u| 1 |]))]
        lcMins = minLeadingCoef <$> (fmap . fmap) snd css

        lead :: [[(Pilot, TrackLead LeadingArea1Units)]] =
                sortOn ((\TrackLead{coef = LeadingCoef c} -> c) . snd)
                <$>
                [
                    [
                        ( p
                        , TrackLead
                            { area = sum1Areas a
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

sum1Areas
    :: LeadingAreas (LeadingArea (Quantity Double [u| km*s |])) (LeadingArea (Quantity Double [u| km*s |]))
    -> (LeadingArea (Quantity Double [u| km*s |]))
sum1Areas LeadingAreas{areaFlown = LeadingArea af, areaAfterLanding = LeadingArea al} =
    LeadingArea $ af +: al
