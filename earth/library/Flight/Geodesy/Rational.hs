{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Geodesy.Rational () where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (LatLng(..), AzimuthFwd, AzimuthRev)
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Distance (QTaskDistance, SpanLatLng, PathDistance)
import Flight.Zone (Zone(..), QRadius)
import Flight.Zone.Path (distancePointToPoint)
import Flight.Zone.Cylinder (TrueCourse(..), CircumSample)
import Flight.Geodesy
    ( EarthMath(..), EarthModel(..)
    , InverseProblem(..), InverseSolution(..)
    , DirectProblem(..), DirectSolution(..)
    )
import Flight.Earth.Ellipsoid
    (GeodeticInverse(..), GeodeticDirect(..), GeodeticAccuracy(..))
import qualified Flight.Earth.Ellipsoid as E (Andoyer(..))
import qualified Internal.Ellipsoid.Separated as E
import qualified Internal.Flat.Separated as P
import qualified Internal.Sphere.Separated as H
import qualified Internal.Ellipsoid.Cylinder.Vincenty.Rational as V
import qualified Internal.Ellipsoid.PointToPoint.Andoyer.Rational as A
import qualified Internal.Ellipsoid.PointToPoint.Vincenty.Rational as V
import qualified Internal.Flat.Cylinder.Rational as P
import qualified Internal.Flat.PointToPoint.Rational as P
import qualified Internal.Sphere.Cylinder.Rational as H
import qualified Internal.Sphere.PointToPoint.Rational as H
import Flight.Geodesy.Solution (Trig, GeodesySolutions(..), GeoZones(..))

instance (Real a, Fractional a) => GeodesySolutions Rational a where
    type Earth Rational = (EarthMath, EarthModel Rational, Epsilon)

    azimuthFwd :: Trig Rational a => Earth Rational -> AzimuthFwd Rational
    azimuthFwd (Pythagorus, EarthAsFlat _, _) =
        P.azimuthFwd
    azimuthFwd (Haversines, EarthAsSphere _, eps) =
        H.azimuthFwd eps
    azimuthFwd (Vincenty, EarthAsEllipsoid e, eps) =
        V.azimuthFwd e eps
    azimuthFwd (AndoyerLambert, EarthAsEllipsoid e, eps) =
        A.azimuthFwd E.AndoyerLambert e eps
    azimuthFwd (ForsytheAndoyerLambert, EarthAsEllipsoid e, eps) =
        A.azimuthFwd E.ForsytheAndoyerLambert e eps
    azimuthFwd (FsAndoyer, EarthAsEllipsoid e, eps) =
        A.azimuthFwd E.FsAndoyer e eps
    azimuthFwd _ =
        error "Forward azimuth unexpected combination of Earth math and model."

    azimuthRev :: Trig Rational a => Earth Rational -> AzimuthRev Rational
    azimuthRev (Pythagorus, EarthAsFlat _, _) =
        P.azimuthRev
    azimuthRev (Haversines, EarthAsSphere _, eps) =
        H.azimuthRev eps
    azimuthRev (Vincenty, EarthAsEllipsoid e, eps) =
        V.azimuthRev e eps
    azimuthRev (AndoyerLambert, EarthAsEllipsoid e, eps) =
        A.azimuthRev E.AndoyerLambert e eps
    azimuthRev (ForsytheAndoyerLambert, EarthAsEllipsoid e, eps) =
        A.azimuthRev E.ForsytheAndoyerLambert e eps
    azimuthRev (FsAndoyer, EarthAsEllipsoid e, eps) =
        A.azimuthRev E.FsAndoyer e eps
    azimuthRev _ =
        error "Reverse azimuth unexpected combination of Earth math and model."

    arcLength :: Trig Rational a => Earth Rational -> SpanLatLng Rational
    arcLength (Pythagorus, _, _) =
        P.distance
    arcLength (Haversines, _, eps) =
        H.distance eps
    arcLength (Vincenty, EarthAsEllipsoid e, eps) =
        V.distance e eps
    arcLength (AndoyerLambert, EarthAsEllipsoid e, eps) =
        A.distance E.AndoyerLambert e eps
    arcLength (ForsytheAndoyerLambert, EarthAsEllipsoid e, eps) =
        A.distance E.ForsytheAndoyerLambert e eps
    arcLength (FsAndoyer, EarthAsEllipsoid e, eps) =
        A.distance E.FsAndoyer e eps
    arcLength _ =
        error "Distance unexpected combination of Earth math and model."

    inverse
        :: Trig Rational a
        => Earth Rational
        -> GeodeticAccuracy Rational
        -> InverseProblem (LatLng Rational [u| rad |])
        -> GeodeticInverse
            (InverseSolution
                (QTaskDistance Rational [u| m |])
                (Quantity Rational [u| rad |])
            )
    inverse (Pythagorus, _, _) =
        error "Pythagorus inverse not implemented."
    inverse (Haversines, _, _) =
        error "Haversines inverse not implemented."
    inverse (Vincenty, EarthAsEllipsoid e, eps) =
        V.inverse e eps
    inverse (AndoyerLambert, EarthAsEllipsoid e, eps) =
        A.inverse E.AndoyerLambert e eps
    inverse (ForsytheAndoyerLambert, EarthAsEllipsoid e, eps) =
        A.inverse E.ForsytheAndoyerLambert e eps
    inverse (FsAndoyer, EarthAsEllipsoid e, eps) =
        A.inverse E.FsAndoyer e eps
    inverse _ =
        error "Inverse unexpected combination of Earth math and model."

    direct
        :: Trig Rational a
        => Earth Rational
        -> GeodeticAccuracy Rational
        -> DirectProblem
            (LatLng Rational [u| rad |])
            (TrueCourse Rational)
            (QRadius Rational [u| m |])
        -> GeodeticDirect
            (DirectSolution
                (LatLng Rational [u| rad |])
                (TrueCourse Rational)
            )
    direct (Pythagorus, EarthAsFlat _, _) =
        error "Direct Pythagorus."
    direct (Haversines, EarthAsSphere _, _) =
        error "Direct Haversines."
    direct (Vincenty, EarthAsEllipsoid e, eps) =
        V.direct e eps
    direct (AndoyerLambert, EarthAsEllipsoid _, _) =
        error "Direct Andoyer-Lambert."
    direct (ForsytheAndoyerLambert, EarthAsEllipsoid _, _) =
        error "Direct Forsythe-Andoyer-Lambert."
    direct _ =
        error "Direct unexpected combination of Earth math and model."

instance (Real a, Fractional a) => GeoZones Rational a where
    separatedZones
        :: (Real a, Fractional a, Trig Rational a)
        => Earth Rational
        -> [Zone Rational]
        -> Bool
    separatedZones x@(Pythagorus, EarthAsFlat _, _) =
        P.separatedZones
            P.azimuthFwd
            (arcLength @_ @Rational x)
    separatedZones x@(Haversines, EarthAsSphere _, eps) =
        H.separatedZones
            (H.azimuthFwd eps)
            (arcLength @_ @Rational x)
    separatedZones x@(Vincenty, EarthAsEllipsoid e, eps) =
        E.separatedZones
            e
            (V.azimuthFwd e eps)
            (arcLength @_ @Rational x)
    separatedZones x@(AndoyerLambert, EarthAsEllipsoid e, eps) =
        E.separatedZones
            e
            (A.azimuthFwd E.AndoyerLambert e eps)
            (arcLength @_ @Rational x)
    separatedZones x@(ForsytheAndoyerLambert, EarthAsEllipsoid e, eps) =
        E.separatedZones
            e
            (A.azimuthFwd E.ForsytheAndoyerLambert e eps)
            (arcLength @_ @Rational x)
    separatedZones x@(FsAndoyer, EarthAsEllipsoid e, eps) =
        E.separatedZones
            e
            (A.azimuthFwd E.FsAndoyer e eps)
            (arcLength @_ @Rational x)
    separatedZones _ =
        error "Separated zones combination of Earth math and model."

    pathDistance
        :: Trig Rational a
        => Earth Rational
        -> [Zone Rational]
        -> PathDistance Rational
    pathDistance x = distancePointToPoint $ arcLength @Rational @Rational x

    circumSample :: Trig Rational a => Earth Rational -> CircumSample Rational
    circumSample (Pythagorus, EarthAsFlat _, _) =
        P.circumSample
    circumSample (Haversines, EarthAsSphere _, _) =
        H.circumSample
    circumSample (Vincenty, EarthAsEllipsoid _, _) =
        V.circumSample

    -- NOTE: Andoyer's is a method for solving the inverse geodesy problem.  We
    -- still use Vincenty for the direct problem's solution. Other alternatives
    -- might be Sjoberg 2006 and Karney 2013.
    circumSample (AndoyerLambert, _, _) =
        V.circumSample
    circumSample (ForsytheAndoyerLambert, _, _) =
        V.circumSample
    circumSample (FsAndoyer, _, _) =
        V.circumSample
    circumSample _ =
        error "Circumference Sample unexpected combination of Earth math and model."
