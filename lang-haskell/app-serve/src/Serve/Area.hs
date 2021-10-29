module Serve.Area (RawLeadingArea(..)) where

import GHC.Generics (Generic)
import Data.Maybe (catMaybes)
import Control.Monad (join)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.UnitsOfMeasure ((-:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (TaskDistance(..), QTaskDistance)
import Flight.Track.Time (TickRow(..), LeadTick(..))
import "flight-gap-lead" Flight.Score
    (LeadingArea(..), LeadingAreas(..), LeadingArea2Units, EssTime(..))
import Flight.Track.Mask (RaceTime(..))

data RawLeadingArea =
    RawLeadingArea
        { raceTime :: Maybe RaceTime
        , raceDistance :: Maybe (QTaskDistance Double [u| m |])
        -- ^ The distance of the speed section.
        , ticks :: [TickRow]
        , areaWithDistanceSquared :: LeadingAreas (LeadingArea LeadingArea2Units) (LeadingArea LeadingArea2Units)
        }
    deriving (Eq, Ord, Generic)

instance ToJSON RawLeadingArea where
    toJSON
        RawLeadingArea
            { raceTime = t
            , raceDistance = d
            , ticks = xs
            , areaWithDistanceSquared =
                LeadingAreas
                    { areaFlown = af
                    , areaAfterLanding = al
                    , areaBeforeStart = bs
                    }
            } = object
        [ "race-distance" .= toJSON d
        , "lead-all-down" .= toJSON down
        -- When the last pilot lands, seconds from the time of first lead.
        , "distance-time" .= toJSON ys
        , "distance-time-after-landing" .= toJSON (distanceTimeAfterDown down d ys)
        , "distance-time-before-start" .= toJSON (distanceTimeBeforeStart d ys)
        , "area-flown" .= toJSON af
        , "area-after-landing" .= toJSON al
        , "area-before-start" .= toJSON bs
        ]
        where
            down = join $ leadAllDown <$> t
            ys = catMaybes $ mkDistanceTime d <$> xs

-- | The area added before the pilot starts if they were not the first to start.
distanceTimeBeforeStart
    ::  Maybe (QTaskDistance Double [u| m |])
    -- ^ The speed section distance.
    -> [[Double]]
    -> [[Double]]
distanceTimeBeforeStart Nothing xs = xs
distanceTimeBeforeStart (Just (TaskDistance td)) xs =
    case xs of
        [_, t] : _ ->
            let (MkQuantity dMax) :: Quantity Double [u| km |] = convert td
            in if t > 0 then [[0, t], [dMax, t]] else []

        _ -> []

-- | The area added after the pilot lands. On the graph this steps up.
distanceTimeAfterDown
    :: Maybe EssTime
    -- ^ The time of the last pilot down.
    -> Maybe (QTaskDistance Double [u| m |])
    -- ^ The speed section distance.
    -> [[Double]]
    -> [[Double]]
distanceTimeAfterDown Nothing _ xs = xs
distanceTimeAfterDown _ Nothing xs = xs
distanceTimeAfterDown (Just (EssTime tMax)) (Just (TaskDistance td)) xs =
    case reverse xs of
        x@[d, _] : _ ->
            let (MkQuantity dMax) :: Quantity Double [u| km |] = convert td
                tMax' = fromRational tMax
            in
                if d >= dMax then [] else [x, [d, tMax'], [dMax, tMax']]

        _ -> []

mkDistanceTime :: Maybe (QTaskDistance Double [u| m |]) -> TickRow -> Maybe [Double]
mkDistanceTime Nothing _ = Nothing
mkDistanceTime _ TickRow{tickLead = Nothing} = Nothing
mkDistanceTime (Just (TaskDistance td)) TickRow{tickLead = Just (LeadTick t), togo}
    | t < 0 = Nothing
    | otherwise =
        let togoKm :: Quantity Double [u| km |] = MkQuantity togo
            (MkQuantity d) :: Quantity Double [u| km |] = convert td -: togoKm

        in if d < 0 then Nothing else Just [d, t]
