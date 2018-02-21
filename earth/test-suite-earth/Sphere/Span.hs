module Sphere.Span (spanD, spanR, sepD, sepR) where

import Flight.LatLng.Rational (defEps)
import Flight.Distance (SpanLatLng)
import Flight.Zone (Zone(..))
import qualified Flight.Earth.Sphere.PointToPoint.Double as Dbl
    (distanceHaversine)
import qualified Flight.Earth.Sphere.PointToPoint.Rational as Rat
    (distanceHaversine)
import qualified Flight.Earth.Sphere.Separated as S (separatedZones)

spanD :: SpanLatLng Double
spanD = Dbl.distanceHaversine

spanR :: SpanLatLng Rational
spanR = Rat.distanceHaversine defEps

sepD :: [Zone Double] -> Bool
sepD = S.separatedZones spanD

sepR :: [Zone Rational] -> Bool
sepR = S.separatedZones spanR
