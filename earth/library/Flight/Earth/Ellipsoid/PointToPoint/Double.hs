module Flight.Earth.Ellipsoid.PointToPoint.Double
    ( distanceAndoyer
    , inverseAndoyer
    , azimuthFwdAndoyer
    , azimuthRevAndoyer

    , distanceVincenty
    , inverseVincenty
    , azimuthFwdVincenty
    , azimuthRevVincenty
    ) where

import Data.UnitsOfMeasure (KnownUnit, Unpack, u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (QLat, QLng, LatLng(..), AzimuthFwd, AzimuthRev)
import Flight.Distance (QTaskDistance, SpanLatLng)
import Flight.Earth.Ellipsoid
    ( Ellipsoid(..), Andoyer(..)
    , GeodeticInverse(..), GeodeticAccuracy(..)
    )
import Flight.Earth.Geodesy (InverseProblem(..), InverseSolution(..))
import qualified Flight.Earth.Ellipsoid.PointToPoint.Andoyer.Double as A
import qualified Flight.Earth.Ellipsoid.PointToPoint.Vincenty.Double as V

distanceAndoyer
    :: ( RealFloat a, Show a
       , KnownUnit (Unpack u), Show (QLat a u), Show (QLng a u)
       , u ~ [u| rad |]
       )
    => Andoyer
    -> Ellipsoid a
    -> SpanLatLng a
distanceAndoyer = A.distance

inverseAndoyer
    :: (Num a, Floating a, Fractional a, RealFloat a, Show a)
    => Andoyer
    -> Ellipsoid a
    -> GeodeticAccuracy a
    -> InverseProblem (LatLng a [u| rad |])
    -> GeodeticInverse
        (InverseSolution
            (QTaskDistance a [u| m |])
            (Quantity a [u| rad |])
        )
inverseAndoyer = A.inverse

azimuthFwdAndoyer
    :: (RealFloat a, Show a)
    => Andoyer
    -> Ellipsoid a
    -> AzimuthFwd a
azimuthFwdAndoyer = A.azimuthFwd

azimuthRevAndoyer
    :: (RealFloat a, Show a)
    => Andoyer
    -> Ellipsoid a
    -> AzimuthRev a
azimuthRevAndoyer = A.azimuthRev

distanceVincenty
    :: ( RealFloat a, Show a
       , KnownUnit (Unpack u), Show (QLat a u), Show (QLng a u)
       , u ~ [u| rad |]
       )
    => Ellipsoid a
    -> SpanLatLng a
distanceVincenty = V.distance

inverseVincenty
    :: (Num a, Floating a, Fractional a, RealFloat a, Show a)
    => Ellipsoid a
    -> GeodeticAccuracy a
    -> InverseProblem (LatLng a [u| rad |])
    -> GeodeticInverse
        (InverseSolution
            (QTaskDistance a [u| m |])
            (Quantity a [u| rad |])
        )
inverseVincenty = V.inverse

azimuthFwdVincenty :: (RealFloat a, Show a) => Ellipsoid a -> AzimuthFwd a
azimuthFwdVincenty = V.azimuthFwd

azimuthRevVincenty :: (RealFloat a, Show a) => Ellipsoid a -> AzimuthRev a
azimuthRevVincenty = V.azimuthRev
