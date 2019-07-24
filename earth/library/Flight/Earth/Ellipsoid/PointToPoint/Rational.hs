module Flight.Earth.Ellipsoid.PointToPoint.Rational
    ( distanceAndoyer
    , inverseAndoyer
    , azimuthFwdAndoyer
    , azimuthRevAndoyer

    , distanceVincenty
    , inverseVincenty
    , azimuthFwdVincenty
    , azimuthRevVincenty
    ) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (LatLng(..), AzimuthFwd, AzimuthRev)
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Distance (QTaskDistance, SpanLatLng)
import Flight.Earth.Ellipsoid
    ( Ellipsoid(..), Andoyer
    , GeodeticInverse(..), GeodeticAccuracy(..)
    )
import Flight.Earth.Geodesy (InverseProblem(..), InverseSolution(..))
import qualified Flight.Earth.Ellipsoid.PointToPoint.Andoyer.Rational as A
import qualified Flight.Earth.Ellipsoid.PointToPoint.Vincenty.Rational as V

distanceAndoyer
    :: (Real a, Fractional a, Show a)
    => Andoyer
    -> Epsilon
    -> Ellipsoid a
    -> SpanLatLng a
distanceAndoyer = A.distance

inverseAndoyer
    :: Andoyer
    -> Epsilon
    -> Ellipsoid Rational
    -> GeodeticAccuracy Rational
    -> InverseProblem (LatLng Rational [u| rad |])
    -> GeodeticInverse
        (InverseSolution
            (QTaskDistance Rational [u| m |])
            (Quantity Rational [u| rad |])
        )
inverseAndoyer = A.inverse

azimuthFwdAndoyer
    :: (Real a, Fractional a, Show a)
    => Andoyer
    -> Epsilon
    -> Ellipsoid a
    -> AzimuthFwd a
azimuthFwdAndoyer = A.azimuthFwd

azimuthRevAndoyer
    :: (Real a, Fractional a, Show a)
    => Andoyer
    -> Epsilon
    -> Ellipsoid a
    -> AzimuthRev a
azimuthRevAndoyer = A.azimuthRev

distanceVincenty
    :: (Real a, Fractional a, Show a)
    => Epsilon
    -> Ellipsoid a
    -> SpanLatLng a
distanceVincenty = V.distance

inverseVincenty
    :: Epsilon
    -> Ellipsoid Rational
    -> GeodeticAccuracy Rational
    -> InverseProblem (LatLng Rational [u| rad |])
    -> GeodeticInverse
        (InverseSolution
            (QTaskDistance Rational [u| m |])
            (Quantity Rational [u| rad |])
        )
inverseVincenty = V.inverse

azimuthFwdVincenty
    :: (Real a, Fractional a, Show a)
    => Epsilon
    -> Ellipsoid a
    -> AzimuthFwd a
azimuthFwdVincenty = V.azimuthFwd

azimuthRevVincenty
    :: (Real a, Fractional a, Show a)
    => Epsilon
    -> Ellipsoid a
    -> AzimuthRev a
azimuthRevVincenty = V.azimuthRev
