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
        , "distance-time" .= toJSON (stepUp down d . catMaybes $ mkDistanceTime d <$> xs)
        ]
        where
            down = join $ leadAllDown <$> t

stepUp :: Maybe EssTime -> Maybe (QTaskDistance Double [u| m |]) -> [[Double]] -> [[Double]]
stepUp Nothing _ xs = xs
stepUp _ Nothing xs = xs
stepUp (Just (EssTime tMax)) (Just (TaskDistance td)) xs =
    case reverse xs of
        [d, _] : _ ->
            let (MkQuantity dMax) :: Quantity Double [u| km |] = convert td
                tMax' = fromRational tMax
            in
                if d >= dMax then xs else xs ++ [[d, tMax'], [dMax, tMax']]

        _ -> xs

mkDistanceTime :: Maybe (QTaskDistance Double [u| m |]) -> TickRow -> Maybe [Double]
mkDistanceTime Nothing _ = Nothing
mkDistanceTime _ TickRow{tickLead = Nothing} = Nothing
mkDistanceTime (Just (TaskDistance td)) TickRow{tickLead = Just (LeadTick t), togo}
    | t < 0 = Nothing
    | otherwise =
        let togoKm :: Quantity Double [u| km |] = MkQuantity togo
            (MkQuantity d) :: Quantity Double [u| km |] = convert td -: togoKm

        in if d < 0 then Nothing else Just [d, t]
