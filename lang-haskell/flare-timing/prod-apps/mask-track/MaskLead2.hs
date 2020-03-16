module MaskLead2 (mask2Lead) where

import Data.Maybe (catMaybes)
import Data.UnitsOfMeasure (u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Comp
    ( Pilot(..)
    , TaskRouteDistance(..)
    , MadeGoal(..)
    , LandedOut(..)
    )
import Flight.Distance
    (QTaskDistance, TaskDistance(..), unTaskDistanceAsKm)
import Flight.Comp.Distance (compDistance)
import Flight.Track.Lead (DiscardingLead(..))
import Flight.Track.Time (LeadTick)
import qualified Flight.Track.Time as Time (TickRow(..))
import Flight.Track.Lead2 (comp2Leading)
import Flight.Track.Mask (MaskingLead(..), RaceTime(..))
import Flight.Score
    ( BestTime(..), MinimumDistance(..), LengthOfSs(..)
    , LeadingArea2Units, area2toCoef
    )

mask2Lead
    :: (MinimumDistance (Quantity Double [u| km |]))
    -> [Maybe RaceTime]
    -> [Maybe TaskRouteDistance]
    -> [[Pilot]]
    -> [[Pilot]]
    -> [Maybe (BestTime (Quantity Double [u| h |]))]
    -> [[Maybe (Pilot, Time.TickRow)]]
    -> DiscardingLead LeadingArea2Units
    -> ( [Maybe (QTaskDistance Double [u| m |])]
       , [[Maybe (Pilot, Maybe LeadTick)]]
       , MaskingLead
       )
mask2Lead
    free
    raceTime
    lsTask
    psArriving
    psLandingOut
    gsBestTime
    rows
    discardingLead =
    (dsBest, rowTicks,) $
    MaskingLead
        { raceTime = raceTime
        , raceDistance = lsSpeedSubset
        , sumDistance = dsSum
        , leadAreaToCoef = lcAreaToCoef
        , leadCoefMin = lcMin
        , leadRank = lead
        }
    where
        lsWholeTask = (fmap . fmap) wholeTaskDistance lsTask
        lsSpeedSubset = (fmap . fmap) speedSubsetDistance lsTask
        (lcMin, lead) = comp2Leading discardingLead lsSpeedSubset

        lcAreaToCoef =
                [
                    area2toCoef
                    . LengthOfSs
                    . (\(TaskDistance d) -> convert . toRational' $ d)
                    <$> ssLen
                | ssLen <- lsSpeedSubset
                ]

        (dsSumArriving, dsSumLandingOut, dsBest, rowTicks) =
                compDistance
                    free
                    lsWholeTask
                    (MadeGoal <$> psArriving)
                    (LandedOut <$> psLandingOut)
                    gsBestTime
                    rows

        -- NOTE: This is the sum of distance over minimum distance.
        dsSum =
                [
                    (fmap $ TaskDistance . MkQuantity)
                    . (\case 0 -> Nothing; x -> Just x)
                    . sum
                    . fmap unTaskDistanceAsKm
                    . catMaybes
                    $ [aSum, lSum]
                | aSum <- dsSumArriving
                | lSum <- dsSumLandingOut
                ]
