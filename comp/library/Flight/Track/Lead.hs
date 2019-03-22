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
    , compLeading
    , lwScalingDefault
    ) where

import "newtype" Control.Newtype (Newtype(..))
import Data.Maybe (catMaybes)
import Data.List (sortOn)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (QTaskDistance)
import Flight.Comp (Pilot, Task(..))
import Flight.Score
    ( LeadingArea(..), LeadingCoef(..), LeadingFraction(..)
    , LwScaling(..)
    , leadingFraction, areaToCoef, mkCoef
    )
import Flight.Track.Time (taskToLeading, leadingAreaSum, minLeadingCoef)
import qualified Flight.Track.Time as Time (TickRow(..))
import Flight.Zone.MkZones (Discipline(..))

data TrackLead =
    TrackLead
        { area :: LeadingArea (Quantity Double [u| (km^2)*s |])
        , coef :: LeadingCoef (Quantity Double [u| 1 |])
        , frac :: LeadingFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

compLeading
    :: [[(Pilot, [Time.TickRow])]]
    -> [Maybe (QTaskDistance Double [u| m |])]
    -> [Task k]
    ->
        ( [Maybe (LeadingCoef (Quantity Double [u| 1 |]))]
        , [[(Pilot, TrackLead)]]
        )
compLeading rowsLeadingStep lsTask tasks' =
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

        ass' :: [[(Pilot, Maybe (LeadingArea (Quantity Double [u| (km^2)*s |])))]] =
                [ (fmap . fmap) (leadingAreaSum l s) xs
                | l <- (fmap . fmap) taskToLeading lsTask
                | s <- speedSection <$> tasks'
                | xs <- rowsLeadingStep
                ]

        ass :: [[(Pilot, LeadingArea (Quantity Double [u| (km^2)*s |]))]] =
                catMaybes
                <$> (fmap . fmap) floatMaybe ass'

        css :: [[(Pilot, LeadingCoef (Quantity Double [u| 1 |]))]] =
                [ (fmap $ LeadingCoef . k . toRational' . unpack) <$> as
                | k <- ks
                | as <- ass
                ]

        lcMins :: [Maybe (LeadingCoef (Quantity Double [u| 1 |]))]
        lcMins = minLeadingCoef <$> (fmap . fmap) snd css

        lead :: [[(Pilot, TrackLead)]] =
                sortOn ((\TrackLead{coef = LeadingCoef c} -> c) . snd)
                <$>
                [
                    [
                        ( p
                        , TrackLead
                            { area = a
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

floatMaybe :: (a, Maybe b) -> Maybe (a, b)
floatMaybe (_, Nothing) = Nothing
floatMaybe (a, Just b) = Just (a, b)

-- | The default explicit leading weight scaling for each discipline?
lwScalingDefault :: Discipline -> LwScaling
lwScalingDefault HangGliding = LwScaling 1
lwScalingDefault Paragliding = LwScaling 2
