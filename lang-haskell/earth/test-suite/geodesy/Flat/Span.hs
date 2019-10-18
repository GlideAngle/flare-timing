module Flat.Span (spanD, spanR, azFwdD, azRevD, sepD, sepR) where

import Data.Ratio ((%))

import Flight.Distance (SpanLatLng)
import Flight.LatLng (AzimuthFwd, AzimuthRev)
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Zone (Zone(..))
import Flight.Geodesy (EarthModel(..), EarthMath(..), Projection(..))
import Flight.Geodesy.Solution (GeodesySolutions(..), GeoZones(..))
import Flight.Geodesy.Double ()
import Flight.Geodesy.Rational ()

eps :: Epsilon
eps = Epsilon $ 1 % 1000000000000000000

spanD :: SpanLatLng Double
spanD = arcLength @Double @Double (Pythagorus, EarthAsFlat UTM)

spanR :: SpanLatLng Rational
spanR = arcLength @Rational @Rational (Pythagorus, EarthAsFlat UTM, eps)

azFwdD :: AzimuthFwd Double
azFwdD = azimuthFwd @Double @Double (Pythagorus, EarthAsFlat UTM)

azRevD :: AzimuthRev Double
azRevD = azimuthRev @Double @Double (Pythagorus, EarthAsFlat UTM)

sepD :: [Zone Double] -> Bool
sepD = separatedZones @Double @Double (Pythagorus, EarthAsFlat UTM)

sepR :: [Zone Rational] -> Bool
sepR = separatedZones @Rational @Rational (Pythagorus, EarthAsFlat UTM, eps)
