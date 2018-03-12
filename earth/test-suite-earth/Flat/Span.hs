module Flat.Span (spanD, spanR, azFwdD, azRevD) where

import Flight.LatLng (AzimuthFwd, AzimuthRev)
import Flight.Distance (SpanLatLng)
import qualified Flight.Earth.Flat.PointToPoint.Double as Dbl
    (distanceEuclidean, azimuthFwd, azimuthRev)
import qualified Flight.Earth.Flat.PointToPoint.Rational as Rat
    (distanceEuclidean)

spanD :: SpanLatLng Double
spanD = Dbl.distanceEuclidean

spanR :: SpanLatLng Rational
spanR = Rat.distanceEuclidean

azFwdD :: AzimuthFwd Double
azFwdD = Dbl.azimuthFwd

azRevD :: AzimuthRev Double
azRevD = Dbl.azimuthRev
