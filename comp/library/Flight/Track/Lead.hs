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

import Data.Maybe (catMaybes)
import Data.List (sortOn)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (QTaskDistance)
import Flight.Comp (Pilot, Task(..))
import Flight.Score
    ( LeadingArea(..), LeadingFraction(..), LwScaling(..)
    , leadingFraction
    )
import Flight.Track.Time (taskToLeading, leadingSum, minLeading)
import qualified Flight.Track.Time as Time (TickRow(..))
import Flight.Zone.MkZones (Discipline(..))

data TrackLead =
    TrackLead
        { coef :: LeadingArea (Quantity Double [u| (km^2)*s |])
        , frac :: LeadingFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

compLeading
    :: [[(Pilot, [Time.TickRow])]]
    -> [Maybe (QTaskDistance Double [u| m |])]
    -> [Task k]
    ->
        ( [Maybe (LeadingArea (Quantity Double [u| (km^2)*s |]))]
        , [[(Pilot, TrackLead)]]
        )
compLeading rowsLeadingStep lsTask tasks' =
    (lcMins, lead)
    where
        rowsLeadingSum' :: [[(Pilot, Maybe (LeadingArea (Quantity Double [u| (km^2)*s |])))]] =
                    [ (fmap . fmap) (leadingSum l s) xs
                    | l <- (fmap . fmap) taskToLeading lsTask
                    | s <- speedSection <$> tasks'
                    | xs <- rowsLeadingStep
                    ]

        rowsLeadingSum :: [[(Pilot, LeadingArea (Quantity Double [u| (km^2)*s |]))]] =
                catMaybes
                <$> (fmap . fmap) floatMaybe rowsLeadingSum'

        lcMins :: [Maybe (LeadingArea (Quantity Double [u| (km^2)*s |]))]
        lcMins =
                minLeading
                <$> (fmap . fmap) snd rowsLeadingSum

        lead :: [[(Pilot, TrackLead)]] =
                sortOn ((\TrackLead{coef = LeadingArea c} -> c) . snd)
                <$>
                [(fmap . fmap)
                    (\lc ->
                        TrackLead
                            { coef = lc
                            , frac =
                                maybe
                                    (LeadingFraction 0)
                                    (`leadingFraction` lc)
                                    lcMin
                            })
                    xs
                | lcMin <- lcMins
                | xs <- rowsLeadingSum
                ]

floatMaybe :: (a, Maybe b) -> Maybe (a, b)
floatMaybe (_, Nothing) = Nothing
floatMaybe (a, Just b) = Just (a, b)

-- | The default explicit leading weight scaling for each discipline?
lwScalingDefault :: Discipline -> LwScaling
lwScalingDefault HangGliding = LwScaling 1
lwScalingDefault Paragliding = LwScaling 2
