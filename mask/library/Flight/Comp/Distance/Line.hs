module Flight.Comp.Distance.Line
     ( DashPathInputs(..)
     , distanceOnlyLine
     , tickNighTrackLine
     , fromKm
     ) where

import Prelude hiding (span)
import Data.Time.Clock (UTCTime)
import Data.Map.Strict (Map)
import Data.UnitsOfMeasure ((-:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Kml (MarkedFixes(..))
import Flight.Distance (QTaskDistance, TaskDistance(..))
import Flight.Comp (Pilot, Task(..))
import Flight.Route (TrackLine(..))
import Flight.Track.Distance (TrackDistance(..), Nigh)
import qualified Flight.Track.Time as Time (TickRow(..))

import Flight.Mask.Internal.Race (Ticked, FlyCut(..))

data DashPathInputs k =
    DashPathInputs
        { dashTask :: Maybe (Task k)
        , dashTicked :: Ticked
        , dashFlyCut :: Maybe (FlyCut UTCTime MarkedFixes)
        }

fromKm :: Double -> Quantity Double [u| m |]
fromKm d =
    convert d'
    where
        d' :: Quantity Double [u| km |]
        d' = MkQuantity d

tickNighTrackLine
    :: Maybe (QTaskDistance Double [u| m |])
    -> Map Pilot (DashPathInputs k)
    -> (Pilot, Time.TickRow)
    -> (Pilot, TrackDistance Nigh)

tickNighTrackLine Nothing _ (p, Time.TickRow{togo = d}) =
    (p,) TrackDistance
        { togo = Just . distanceOnlyLine . fromKm $ d
        , made = Nothing
        }

tickNighTrackLine
    (Just (TaskDistance td))
    _
    (p, Time.TickRow{togo = d}) =
    (p,) TrackDistance
        { togo = Just line
        , made = Just . TaskDistance $ td -: togo'
        }
    where
        togo' = fromKm d
        line = distanceOnlyLine togo'

distanceOnlyLine :: Quantity Double [u| m |] -> TrackLine
distanceOnlyLine d =
    TrackLine
        { distance = TaskDistance d
        , waypoints = []
        , legs = []
        , legsSum = []
        , flipSum = []
        }
