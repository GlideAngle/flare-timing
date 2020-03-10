module ServeArea (RawLeadingArea(..)) where

import GHC.Generics (Generic)
import Data.Maybe (catMaybes)
import Control.Monad (join)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.UnitsOfMeasure ((-:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (TaskDistance(..), QTaskDistance)
import Flight.Track.Time (TickRow(..), LeadTick(..))
import Flight.Score (EssTime(..))
import Flight.Track.Mask (RaceTime(..))

data RawLeadingArea =
    RawLeadingArea
        { raceTime :: Maybe RaceTime
        , raceDistance :: Maybe (QTaskDistance Double [u| m |])
        -- ^ The distance of the speed section.
        , ticks :: [TickRow]
        }
    deriving (Eq, Ord, Generic)

instance ToJSON RawLeadingArea where
    toJSON RawLeadingArea{raceTime = t, raceDistance = d, ticks = xs} = object
        [ "race-distance" .= toJSON d
        , "lead-all-down" .= toJSON down
        -- When the last pilot lands, seconds from the time of first lead.
        , "distance-time" .= toJSON ys
        , "area-after-down" .= toJSON (areaAfterDown down d ys)
        , "area-before-start" .= toJSON (areaBeforeStart d ys)
        ]
        where
            down = join $ leadAllDown <$> t
            ys = catMaybes $ mkDistanceTime d <$> xs

-- | The area added before the pilot starts if they were not the first to start.
areaBeforeStart
    ::  Maybe (QTaskDistance Double [u| m |])
    -- ^ The speed section distance.
    -> [[Double]]
    -> [[Double]]
areaBeforeStart Nothing xs = xs
areaBeforeStart (Just (TaskDistance td)) xs =
    case xs of
        [_, t] : _ ->
            let (MkQuantity dMax) :: Quantity Double [u| km |] = convert td
            in if t > 0 then [[0, t], [dMax, t]] else []

        _ -> []

-- | The area added after the pilot lands. On the graph this steps up.
areaAfterDown
    :: Maybe EssTime
    -- ^ The time of the last pilot down.
    -> Maybe (QTaskDistance Double [u| m |])
    -- ^ The speed section distance.
    -> [[Double]]
    -> [[Double]]
areaAfterDown Nothing _ xs = xs
areaAfterDown _ Nothing xs = xs
areaAfterDown (Just (EssTime tMax)) (Just (TaskDistance td)) xs =
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
