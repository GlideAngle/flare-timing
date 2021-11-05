module MaskLeadCoef (maskLeadCoef) where

import Data.Maybe (catMaybes)
import Data.UnitsOfMeasure (KnownUnit, Unpack, u, convert, toRational')
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
import Flight.Track.Lead (CompLeading(..), LeadingAreaSum, MkLeadingCoef, MkAreaToCoef, compLeading)
import Flight.Track.Time (LeadTick)
import qualified Flight.Track.Time as Time (TickRow(..))
import Flight.Track.Mask (CompMaskingLead(..), RaceTime(..))
import "flight-gap-allot" Flight.Score (BestTime(..), MinimumDistance(..))
import "flight-gap-lead" Flight.Score (LeadingAreaUnits, LengthOfSs(..))

maskLeadCoef
    :: (KnownUnit (Unpack u))
    => LeadingAreaSum u
    -> MkLeadingCoef u
    -> MkAreaToCoef v
    -> MinimumDistance (Quantity Double [u| km |])
    -> [Maybe RaceTime]
    -> [Maybe TaskRouteDistance]
    -> [[Pilot]]
    -> [[Pilot]]
    -> [Maybe (BestTime (Quantity Double [u| h |]))]
    -> [[Maybe (Pilot, Time.TickRow)]]
    -> CompLeading (LeadingAreaUnits u)
    -> ( [Maybe (QTaskDistance Double [u| m |])]
       , [[Maybe (Pilot, Maybe LeadTick)]]
       , CompMaskingLead u v
       )
maskLeadCoef
    sumAreas
    invert
    areaToCoef
    free
    raceTime
    lsTask
    psArriving
    psLandingOut
    gsBestTime
    rows
    discardingLead =
    (dsBest, rowTicks,) $
    CompMaskingLead
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
        (lcMin, lead) = compLeading sumAreas invert discardingLead lsSpeedSubset

        lcAreaToCoef =
                [
                    areaToCoef
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
                    fmap (TaskDistance . MkQuantity)
                    . (\case 0 -> Nothing; x -> Just x)
                    . sum
                    . fmap unTaskDistanceAsKm
                    . catMaybes
                    $ [aSum, lSum]
                | aSum <- dsSumArriving
                | lSum <- dsSumLandingOut
                ]
