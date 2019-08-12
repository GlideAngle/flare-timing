module Sphere.Span (spanD, spanR, azFwdD, azRevD, sepD, sepR) where

import Flight.LatLng.Rational (defEps)
import Flight.LatLng (AzimuthFwd, AzimuthRev)
import Flight.Distance (SpanLatLng)
import Flight.Zone (Zone(..))
import Flight.Earth.Sphere (earthRadius)
import Flight.Geodesy (EarthModel(..), EarthMath(..))
import Flight.Geodesy.Solution (GeodesySolutions(..), GeoZones(..))
import Flight.Geodesy.Double ()
import Flight.Geodesy.Rational ()

spanD :: SpanLatLng Double
spanD = arcLength @Double @Double (Haversines, EarthAsSphere earthRadius)

spanR :: SpanLatLng Rational
spanR = arcLength @Rational @Rational (Haversines, EarthAsSphere earthRadius, defEps)

azFwdD :: AzimuthFwd Double
azFwdD = azimuthFwd @Double @Double (Haversines, EarthAsSphere earthRadius)

azRevD :: AzimuthRev Double
azRevD = azimuthRev @Double @Double (Haversines, EarthAsSphere earthRadius)

sepD :: [Zone Double] -> Bool
sepD = separatedZones @Double @Double (Haversines, EarthAsSphere earthRadius)

sepR :: [Zone Rational] -> Bool
sepR = separatedZones @Rational @Rational (Haversines, EarthAsSphere earthRadius, defEps)
