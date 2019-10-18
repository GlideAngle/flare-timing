module Flight.Geodesy.Solution where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (LatLng(..), AzimuthFwd, AzimuthRev)
import Flight.Distance (QTaskDistance, SpanLatLng, PathDistance)
import Flight.Zone (Zone(..), QRadius)
import Flight.Zone.Cylinder (TrueCourse(..), CircumSample)
import Flight.Earth.Ellipsoid
    (GeodeticInverse(..), GeodeticDirect(..), GeodeticAccuracy(..))
import Flight.Geodesy.Problem
    ( InverseProblem(..), InverseSolution(..)
    , DirectProblem(..), DirectSolution(..)
    )

import GHC.Exts (Constraint)

type family Trig g a :: Constraint
type instance Trig Rational a = (Real a, Fractional a)
type instance Trig Double a = RealFloat a

class (Real a, Fractional a, Trig g a) => GeodesySolutions g a where
    type Earth g :: *

    azimuthFwd :: Trig g a => Earth g -> AzimuthFwd g
    azimuthRev :: Trig g a => Earth g -> AzimuthRev g
    arcLength :: Trig g a => Earth g -> SpanLatLng g

    inverse
        :: Trig g a
        => Earth g
        -> GeodeticAccuracy g
        -> InverseProblem (LatLng g [u| rad |])
        -> GeodeticInverse
            (InverseSolution
                (QTaskDistance g [u| m |])
                (Quantity g [u| rad |])
            )

    direct
        :: Trig g a
        => Earth g
        -> GeodeticAccuracy g
        -> DirectProblem
            (LatLng g [u| rad |])
            (TrueCourse g)
            (QRadius g [u| m |])
        -> GeodeticDirect
            (DirectSolution
                (LatLng g [u| rad |])
                (TrueCourse g)
            )

class GeodesySolutions g a => GeoZones g a where
    separatedZones :: Trig g a => Earth g -> [Zone g] -> Bool
    pathDistance :: Trig g a => Earth g -> [Zone g] -> PathDistance g
    circumSample :: Trig g a => Earth g -> CircumSample g

type SeparatedZones a = [Zone a] -> Bool
