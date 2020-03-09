module ServeArea (RawLeadingArea(..)) where

import GHC.Generics (Generic)
import Data.Maybe (catMaybes)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.UnitsOfMeasure ((-:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (TaskDistance(..), QTaskDistance)
import Flight.Track.Time (TickRow(..), LeadTick(..))

data RawLeadingArea =
    RawLeadingArea
        { raceDistance :: Maybe (QTaskDistance Double [u| m |])
        -- ^ The distance of the speed section.
        , ticks :: [TickRow]
        }
    deriving (Eq, Ord, Generic)

instance ToJSON RawLeadingArea where
    toJSON RawLeadingArea{raceDistance = d, ticks = xs} = object
        [ "race-distance" .= toJSON d
        , "distance-time" .= toJSON (catMaybes $ mkDistanceTime d <$> xs)
        ]

mkDistanceTime :: Maybe (QTaskDistance Double [u| m |]) -> TickRow -> Maybe [Double]
mkDistanceTime Nothing _ = Nothing
mkDistanceTime _ TickRow{tickLead = Nothing} = Nothing
mkDistanceTime (Just (TaskDistance td)) TickRow{tickLead = Just (LeadTick t), togo}
    | t < 0 = Nothing
    | otherwise =
        let togoKm :: Quantity Double [u| km |] = MkQuantity togo
            (MkQuantity d) :: Quantity Double [u| km |] = convert td -: togoKm

        in if d < 0 then Nothing else Just [d, t]
