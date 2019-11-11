{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Geodesy.Double () where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (LatLng(..), AzimuthFwd, AzimuthRev)
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
import qualified Internal.Ellipsoid.Cylinder.Vincenty.Double as V
import qualified Internal.Ellipsoid.PointToPoint.Andoyer.Double as A
import qualified Internal.Ellipsoid.PointToPoint.Vincenty.Double as V
import qualified Internal.Flat.Cylinder.Double as P
import qualified Internal.Flat.PointToPoint.Double as P
import qualified Internal.Sphere.Cylinder.Double as H
import qualified Internal.Sphere.PointToPoint.Double as H
import Flight.Geodesy.Solution (Trig, GeodesySolutions(..), GeoZones(..))

instance RealFloat a => GeodesySolutions Double a where
    type Earth Double = (EarthMath, EarthModel Double)

    azimuthFwd :: Trig Double a => Earth Double -> AzimuthFwd Double
    azimuthFwd (Pythagorus, _) =
        P.azimuthFwd
    azimuthFwd (Haversines, _) =
        H.azimuthFwd
    azimuthFwd (Vincenty, EarthAsEllipsoid e) =
        V.azimuthFwd e
    azimuthFwd (AndoyerLambert, EarthAsEllipsoid e) =
        A.azimuthFwd E.AndoyerLambert e
    azimuthFwd (ForsytheAndoyerLambert, EarthAsEllipsoid e) =
        A.azimuthFwd E.ForsytheAndoyerLambert e
    azimuthFwd (FsAndoyer, EarthAsEllipsoid e) =
        A.azimuthFwd E.FsAndoyer e
    azimuthFwd _ =
        error "Forward azimuth unexpected combination of Earth math and model."

    azimuthRev :: Trig Double a => Earth Double -> AzimuthRev Double
    azimuthRev (Pythagorus, _) =
        P.azimuthRev
    azimuthRev (Haversines, _) =
        H.azimuthRev
    azimuthRev (Vincenty, EarthAsEllipsoid e) =
        V.azimuthRev e
    azimuthRev (AndoyerLambert, EarthAsEllipsoid e) =
        A.azimuthRev E.AndoyerLambert e
    azimuthRev (ForsytheAndoyerLambert, EarthAsEllipsoid e) =
        A.azimuthRev E.ForsytheAndoyerLambert e
    azimuthRev (FsAndoyer, EarthAsEllipsoid e) =
        A.azimuthRev E.FsAndoyer e
    azimuthRev _ =
        error "Reverse azimuth unexpected combination of Earth math and model."

    arcLength :: Trig Double a => Earth Double -> SpanLatLng Double
    arcLength (Pythagorus, EarthAsFlat _) =
        P.distance
    arcLength (Haversines, EarthAsSphere _) =
        H.distance
    arcLength (Vincenty, EarthAsEllipsoid e) =
        V.distance e
    arcLength (AndoyerLambert, EarthAsEllipsoid e) =
        A.distance E.AndoyerLambert e
    arcLength (ForsytheAndoyerLambert, EarthAsEllipsoid e) =
        A.distance E.ForsytheAndoyerLambert e
    arcLength (FsAndoyer, EarthAsEllipsoid e) =
        A.distance E.FsAndoyer e
    arcLength _ =
        error "Distance unexpected combination of Earth math and model."

    inverse
        :: Trig Double a
        => Earth Double
        -> GeodeticAccuracy Double
        -> InverseProblem (LatLng Double [u| rad |])
        -> GeodeticInverse
            (InverseSolution
                (QTaskDistance Double [u| m |])
                (Quantity Double [u| rad |])
            )
    inverse (Pythagorus, EarthAsFlat _) =
        error "Pythagorus inverse not implemented."
    inverse (Haversines, EarthAsSphere _) =
        error "Haversines inverse not implemented."
    inverse (Vincenty, EarthAsEllipsoid e) =
        V.inverse e
    inverse (AndoyerLambert, EarthAsEllipsoid e) =
        A.inverse E.AndoyerLambert e
    inverse (ForsytheAndoyerLambert, EarthAsEllipsoid e) =
        A.inverse E.ForsytheAndoyerLambert e
    inverse (FsAndoyer, EarthAsEllipsoid e) =
        A.inverse E.FsAndoyer e
    inverse _ =
        error "Inverse unexpected combination of Earth math and model."

    direct
        :: Trig Double a
        => Earth Double
        -> GeodeticAccuracy Double
        -> DirectProblem
            (LatLng Double [u| rad |])
            (TrueCourse Double)
            (QRadius Double [u| m |])
        -> GeodeticDirect
            (DirectSolution
                (LatLng Double [u| rad |])
                (TrueCourse Double)
            )
    direct (Pythagorus, EarthAsFlat _) =
        error "Direct Pythagorus."
    direct (Haversines, EarthAsSphere _) =
        const H.direct
    direct (Vincenty, EarthAsEllipsoid e) =
        V.direct e
    direct (AndoyerLambert, EarthAsEllipsoid _) =
        error "Direct Andoyer-Lambert."
    direct (ForsytheAndoyerLambert, EarthAsEllipsoid _) =
        error "Direct Forsythe-Andoyer-Lambert."
    direct _ =
        error "Direct unexpected combination of Earth math and model."

instance RealFloat a => GeoZones Double a where
    separatedZones
        :: Trig Double a
        => Earth Double
        -> [Zone Double]
        -> Bool
    separatedZones x@(Pythagorus, EarthAsFlat _) =
        P.separatedZones P.azimuthFwd (arcLength x)
    separatedZones x@(Haversines, EarthAsSphere _) =
        H.separatedZones H.azimuthFwd (arcLength x)
    separatedZones x@(Vincenty, EarthAsEllipsoid e) =
        E.separatedZones e (V.azimuthFwd e) (arcLength x)
    separatedZones x@(AndoyerLambert, EarthAsEllipsoid e) =
        E.separatedZones e (A.azimuthFwd E.AndoyerLambert e) (arcLength x)
    separatedZones x@(ForsytheAndoyerLambert, EarthAsEllipsoid e) =
        E.separatedZones e (A.azimuthFwd E.ForsytheAndoyerLambert e) (arcLength x)
    separatedZones x@(FsAndoyer, EarthAsEllipsoid e) =
        E.separatedZones e (A.azimuthFwd E.FsAndoyer e) (arcLength x)
    separatedZones _ =
        error "Separated zones combination of Earth math and model."

    pathDistance
        :: Trig Double a
        => Earth Double
        -> [Zone Double]
        -> PathDistance Double
    pathDistance x = distancePointToPoint $ arcLength x

    circumSample :: Trig Double a => Earth Double -> CircumSample Double
    circumSample (Pythagorus, EarthAsFlat _) =
        P.circumSample
    circumSample (Haversines, EarthAsSphere _) =
        H.circumSample
    circumSample (Vincenty, EarthAsEllipsoid _) =
        V.circumSample

    -- NOTE: Andoyer's is a method for solving the inverse geodesy problem.  We
    -- still use Vincenty for the direct problem's solution. Other alternatives
    -- might be Sjoberg 2006 and Karney 2013.
    circumSample (AndoyerLambert, _) =
        V.circumSample
    circumSample (ForsytheAndoyerLambert, _) =
        V.circumSample
    circumSample (FsAndoyer, _) =
        V.circumSample
    circumSample _ =
        error "Circumference Sample unexpected combination of Earth math and model."
