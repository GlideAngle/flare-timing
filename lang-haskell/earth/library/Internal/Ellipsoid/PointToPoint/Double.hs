module Internal.Ellipsoid.PointToPoint.Double
    ( distance
    , inverse
    , azimuthFwd
    , azimuthRev
    ) where

import Data.UnitsOfMeasure (KnownUnit, Unpack, u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (LatLng(..), AzimuthFwd, AzimuthRev)
import Flight.Distance (QTaskDistance, SpanLatLng)
import Flight.Earth.Ellipsoid
    (Ellipsoid(..), GeodeticInverse(..), GeodeticAccuracy(..))
import qualified Flight.Earth.Ellipsoid as E (Andoyer(..))
import Flight.Geodesy (EarthMath(..), InverseProblem(..), InverseSolution(..))
import qualified Internal.Ellipsoid.PointToPoint.Andoyer.Double as A
import qualified Internal.Ellipsoid.PointToPoint.Vincenty.Double as V

distance
    :: ( RealFloat a
       , KnownUnit (Unpack u)
       , u ~ [u| rad |]
       )
    => EarthMath
    -> Ellipsoid a
    -> SpanLatLng a
distance Pythagorus = error "Pythagorus on the Ellipsoid"
distance Haversines = error "Haversines on the Ellipsoid"
distance Vincenty = V.distance
distance AndoyerLambert = A.distance E.AndoyerLambert
distance ForsytheAndoyerLambert = A.distance E.ForsytheAndoyerLambert
distance FsAndoyer = A.distance E.FsAndoyer

inverse
    :: (Num a, Fractional a, RealFloat a)
    => EarthMath
    -> Ellipsoid a
    -> GeodeticAccuracy a
    -> InverseProblem (LatLng a [u| rad |])
    -> GeodeticInverse
        (InverseSolution
            (QTaskDistance a [u| m |])
            (Quantity a [u| rad |])
        )
inverse Pythagorus = error "Pythagorus on the Ellipsoid"
inverse Haversines = error "Haversines on the Ellipsoid"
inverse Vincenty = V.inverse
inverse AndoyerLambert = A.inverse E.AndoyerLambert
inverse ForsytheAndoyerLambert = A.inverse E.ForsytheAndoyerLambert
inverse FsAndoyer = A.inverse E.FsAndoyer

azimuthFwd
    :: RealFloat a
    => EarthMath
    -> Ellipsoid a
    -> AzimuthFwd a
azimuthFwd Pythagorus = error "Pythagorus on the Ellipsoid"
azimuthFwd Haversines = error "Haversines on the Ellipsoid"
azimuthFwd Vincenty = V.azimuthFwd
azimuthFwd AndoyerLambert = A.azimuthFwd E.AndoyerLambert
azimuthFwd ForsytheAndoyerLambert = A.azimuthFwd E.ForsytheAndoyerLambert
azimuthFwd FsAndoyer = A.azimuthFwd E.FsAndoyer

azimuthRev
    :: RealFloat a
    => EarthMath
    -> Ellipsoid a
    -> AzimuthRev a
azimuthRev Pythagorus = error "Pythagorus on the Ellipsoid"
azimuthRev Haversines = error "Haversines on the Ellipsoid"
azimuthRev Vincenty = V.azimuthRev
azimuthRev AndoyerLambert = A.azimuthRev E.AndoyerLambert
azimuthRev ForsytheAndoyerLambert = A.azimuthRev E.ForsytheAndoyerLambert
azimuthRev FsAndoyer = A.azimuthRev E.FsAndoyer
