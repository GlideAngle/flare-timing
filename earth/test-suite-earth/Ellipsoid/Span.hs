module Ellipsoid.Span (spanD, spanR) where

import Flight.LatLng.Rational (defEps)
import Flight.Distance (SpanLatLng)
import qualified Flight.Earth.Ellipsoid.PointToPoint.Double as Dbl
    (distanceVincenty)
import qualified Flight.Earth.Ellipsoid.PointToPoint.Rational as Rat
    (distanceVincenty)
import Flight.Earth.Ellipsoid (wgs84)

spanD :: SpanLatLng Double
spanD = Dbl.distanceVincenty wgs84

spanR :: SpanLatLng Rational
spanR = Rat.distanceVincenty defEps wgs84
