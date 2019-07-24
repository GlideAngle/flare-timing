module Flight.Earth.Ellipsoid.PointToPoint.Rational
    ( distance
    , inverse
    , azimuthFwd
    , azimuthRev
    ) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (LatLng(..), AzimuthFwd, AzimuthRev)
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Distance (QTaskDistance, SpanLatLng)
import Flight.Earth.Ellipsoid
    ( Ellipsoid(..)
    , GeodeticInverse(..), GeodeticAccuracy(..)
    )
import qualified Flight.Earth.Ellipsoid as E (Andoyer(..))
import Flight.Earth.Geodesy
    (EarthMath(..), InverseProblem(..), InverseSolution(..))
import qualified Flight.Earth.Ellipsoid.PointToPoint.Andoyer.Rational as A
import qualified Flight.Earth.Ellipsoid.PointToPoint.Vincenty.Rational as V

distance
    :: (Real a, Fractional a, Show a)
    => EarthMath
    -> Epsilon
    -> Ellipsoid a
    -> SpanLatLng a
distance Pythagorus = error "Pythagorus on the Ellipsoid"
distance Haversines = error "Haversines on the Ellipsoid"
distance Vincenty = V.distance
distance AndoyerLambert = A.distance E.AndoyerLambert
distance ForsytheAndoyerLambert = A.distance E.ForsytheAndoyerLambert

inverse
    :: EarthMath
    -> Epsilon
    -> Ellipsoid Rational
    -> GeodeticAccuracy Rational
    -> InverseProblem (LatLng Rational [u| rad |])
    -> GeodeticInverse
        (InverseSolution
            (QTaskDistance Rational [u| m |])
            (Quantity Rational [u| rad |])
        )
inverse Pythagorus = error "Pythagorus on the Ellipsoid"
inverse Haversines = error "Haversines on the Ellipsoid"
inverse Vincenty = V.inverse
inverse AndoyerLambert = A.inverse E.AndoyerLambert
inverse ForsytheAndoyerLambert = A.inverse E.ForsytheAndoyerLambert

azimuthFwd
    :: (Real a, Fractional a, Show a)
    => EarthMath
    -> Epsilon
    -> Ellipsoid a
    -> AzimuthFwd a
azimuthFwd Pythagorus = error "Pythagorus on the Ellipsoid"
azimuthFwd Haversines = error "Haversines on the Ellipsoid"
azimuthFwd Vincenty = V.azimuthFwd
azimuthFwd AndoyerLambert = A.azimuthFwd E.AndoyerLambert
azimuthFwd ForsytheAndoyerLambert = A.azimuthFwd E.ForsytheAndoyerLambert

azimuthRev
    :: (Real a, Fractional a, Show a)
    => EarthMath
    -> Epsilon
    -> Ellipsoid a
    -> AzimuthRev a
azimuthRev Pythagorus = error "Pythagorus on the Ellipsoid"
azimuthRev Haversines = error "Haversines on the Ellipsoid"
azimuthRev Vincenty = V.azimuthRev
azimuthRev AndoyerLambert = A.azimuthRev E.AndoyerLambert
azimuthRev ForsytheAndoyerLambert = A.azimuthRev E.ForsytheAndoyerLambert
