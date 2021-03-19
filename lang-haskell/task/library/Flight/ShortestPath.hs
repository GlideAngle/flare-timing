module Flight.ShortestPath (GeoPath(..), OptimizePath, OptimalPath) where

import Flight.Zone (Zone(..))
import Flight.Zone.Cylinder (SampleParams, CircumSample, ZonePoint)
import Flight.Units ()
import Flight.Distance (PathDistance(..))
import Flight.Geodesy.Solution (Trig, GeoZones(..), GeodesySolutions(..))
import Flight.ShortestPath.Cost

type OptimalPath a = ([ZonePoint a], PathDistance a)
type OptimizePath a = Maybe [ZonePoint a] -> [Zone a] -> Zs (OptimalPath a)

class GeoZones g a => GeoPath g a where
    shortestPath
        :: Trig g a
        => Earth g
        -> CostSegment g
        -> CircumSample g
        -> AngleCut g
        -> SampleParams g
        -> Maybe [ZonePoint g]
        -> [Zone g]
        -> Zs (OptimalPath g)
