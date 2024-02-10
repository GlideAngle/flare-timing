{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Comp.Distance.Double () where

import Prelude hiding (span)
import Data.Maybe (catMaybes)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (lookup)
import Data.UnitsOfMeasure ((-:), u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (QTaskDistance, TaskDistance(..))
import Flight.Comp (Pilot)
import Flight.Zone.Cylinder (SampleParams(..))
import Flight.Zone.Raw (Give)
import Flight.Route (TrackLine(..), toTrackLine)
import Flight.Track.Distance (TrackDistance(..), Nigh)
import qualified Flight.Track.Time as Time (TimeRow(..))
import qualified Flight.Task as T (fromZs)
import Flight.Geodesy.Solution (Trig, GeodesySolutions(..))

import Flight.Mask.Internal.Race (FlyCut(..))
import Flight.Mask.Distance (GeoDash(..))
import Flight.Comp.Distance (GeoNigh(..), DashPathInputs(..))
import Flight.Comp.Distance.Line (distanceOnlyLine, fromKm)

import Flight.Geodesy.Double ()
import Flight.ShortestPath.Double ()
import Flight.Span.Double ()
import Flight.Mask.Distance.Double ()

instance GeoDash Double a => GeoNigh Double a where
    compNighTime
        :: Trig Double a
        => Earth Double
        -> Maybe Give
        -> SampleParams Double
        -> [Maybe (QTaskDistance Double [u| m |])]
        -> [Map Pilot (DashPathInputs k)]
        -> [[Maybe (Pilot, Time.TimeRow)]]
        -> [[(Pilot, TrackDistance Nigh)]]
    compNighTime e give sp lsTask zsTaskTicked rows =
            [ timeNighTrackLine @Double @Double e give sp td zs <$> xs
            | td <- lsTask
            | zs <- zsTaskTicked
            | xs <- (catMaybes <$> rows)
            ]

    timeNighTrackLine
        :: Trig Double a
        => Earth Double
        -> Maybe Give
        -> SampleParams Double
        -> Maybe (QTaskDistance Double [u| m |])
        -> Map Pilot (DashPathInputs k)
        -> (Pilot, Time.TimeRow)
        -> (Pilot, TrackDistance Nigh)

    timeNighTrackLine _ _ _ Nothing _ (p, Time.TimeRow{togo = d}) =
        (p,) TrackDistance
            { togo = Just . distanceOnlyLine . fromKm $ d
            , made = Nothing
            }

    timeNighTrackLine
        e
        give
        sp
        (Just (TaskDistance td))
        zsTaskTicked
        (p, row@Time.TimeRow{togo = d}) =
        (p,) TrackDistance
            { togo = Just line
            , made = Just . TaskDistance $ td -: togo'
            }
        where
            togo' = fromKm d

            line =
                case Map.lookup p zsTaskTicked of
                    Nothing -> distanceOnlyLine togo'
                    Just dpi -> pathToGo @Double @Double e give sp dpi row togo'

    pathToGo
        :: Trig Double a
        => Earth Double
        -> Maybe Give
        -> SampleParams Double
        -> DashPathInputs k
        -> Time.TimeRow
        -> Quantity Double [u| m |]
        -> TrackLine

    pathToGo e give sp DashPathInputs{..} x@Time.TimeRow{time} d =
        case dashTask of
            Nothing -> distanceOnlyLine d
            Just dashTask' ->
                maybe
                    (distanceOnlyLine d)
                    (toTrackLine span False)
                    (T.fromZs path)
                where
                    span = arcLength @Double @Double e
                    path =
                        dashPathToGoalTimeRows @Double @Double
                            e
                            give
                            sp
                            dashTicked
                            dashTask'
                            FlyCut{cut = Just (time, time), uncut = [x]}
