module Ellipsoid.Span (spanD, spanR, azFwdD, azRevD) where

import Data.Ratio ((%))

import Flight.Distance (SpanLatLng)
import Flight.LatLng (AzimuthFwd, AzimuthRev)
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Earth.Ellipsoid (Ellipsoid)
import qualified Flight.Earth.Ellipsoid.PointToPoint.Double as Dbl
    (distanceVincenty, azimuthFwd, azimuthRev)
import qualified Flight.Earth.Ellipsoid.PointToPoint.Rational as Rat
    (distanceVincenty)

e :: Epsilon
e = Epsilon $ 1 % 1000000000000000000

spanD :: Ellipsoid Double -> SpanLatLng Double
spanD = Dbl.distanceVincenty

spanR :: Ellipsoid Rational -> SpanLatLng Rational
spanR = Rat.distanceVincenty e

azFwdD :: Ellipsoid Double -> AzimuthFwd Double
azFwdD = Dbl.azimuthFwd

azRevD :: Ellipsoid Double -> AzimuthRev Double
azRevD = Dbl.azimuthRev
