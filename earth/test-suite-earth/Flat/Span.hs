module Flat.Span (spanD, spanR) where

import Flight.Distance (SpanLatLng)
import qualified Flight.Earth.Flat.PointToPoint.Double as Dbl (distanceEuclidean)
import qualified Flight.Earth.Flat.PointToPoint.Rational as Rat (distanceEuclidean)

spanD :: SpanLatLng Double
spanD = Dbl.distanceEuclidean

spanR :: SpanLatLng Rational
spanR = Rat.distanceEuclidean
