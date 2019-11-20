module Flight.ShortestPath (GeoPath(..), OptimalPath) where

import Flight.Zone (Zone(..))
import Flight.Zone.Cylinder (SampleParams, CircumSample)
import Flight.Units ()
import Flight.Distance (PathDistance(..))
import Flight.Geodesy.Solution (Trig, GeoZones(..), GeodesySolutions(..))
import Flight.ShortestPath.Cost

type OptimalPath a = [Zone a] -> Zs (PathDistance a)

class GeoZones g a => GeoPath g a where
    shortestPath
        :: Trig g a
        => Earth g
        -> CostSegment g
        -> CircumSample g
        -> AngleCut g
        -> SampleParams g
        -> [Zone g]
        -> Zs (PathDistance g)
