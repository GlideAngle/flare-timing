module Internal.Ellipsoid.PointToPoint.Rational
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
import Flight.Geodesy (EarthMath(..), InverseProblem(..), InverseSolution(..))
import qualified Internal.Ellipsoid.PointToPoint.Andoyer.Rational as A
import qualified Internal.Ellipsoid.PointToPoint.Vincenty.Rational as V

distance
    :: (Real a, Fractional a, Show a)
    => EarthMath
    -> Ellipsoid a
    -> Epsilon
    -> SpanLatLng a
distance Pythagorus = error "Pythagorus on the Ellipsoid"
distance Haversines = error "Haversines on the Ellipsoid"
distance Vincenty = V.distance
distance AndoyerLambert = A.distance E.AndoyerLambert
distance ForsytheAndoyerLambert = A.distance E.ForsytheAndoyerLambert
distance FsAndoyer = A.distance E.FsAndoyer

inverse
    :: EarthMath
    -> Ellipsoid Rational
    -> Epsilon
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
inverse FsAndoyer = A.inverse E.FsAndoyer

azimuthFwd
    :: (Real a, Fractional a, Show a)
    => EarthMath
    -> Ellipsoid a
    -> Epsilon
    -> AzimuthFwd a
azimuthFwd Pythagorus = error "Pythagorus on the Ellipsoid"
azimuthFwd Haversines = error "Haversines on the Ellipsoid"
azimuthFwd Vincenty = V.azimuthFwd
azimuthFwd AndoyerLambert = A.azimuthFwd E.AndoyerLambert
azimuthFwd ForsytheAndoyerLambert = A.azimuthFwd E.ForsytheAndoyerLambert
azimuthFwd FsAndoyer = A.azimuthFwd E.FsAndoyer

azimuthRev
    :: (Real a, Fractional a, Show a)
    => EarthMath
    -> Ellipsoid a
    -> Epsilon
    -> AzimuthRev a
azimuthRev Pythagorus = error "Pythagorus on the Ellipsoid"
azimuthRev Haversines = error "Haversines on the Ellipsoid"
azimuthRev Vincenty = V.azimuthRev
azimuthRev AndoyerLambert = A.azimuthRev E.AndoyerLambert
azimuthRev ForsytheAndoyerLambert = A.azimuthRev E.ForsytheAndoyerLambert
azimuthRev FsAndoyer = A.azimuthRev E.FsAndoyer
