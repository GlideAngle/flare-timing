module Meridian.Sphere (units, unitsR) where

import Test.Tasty (TestTree, testGroup)
import Data.UnitsOfMeasure ((*:), u, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Distance (TaskDistance(..))
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng.Rational (defEps)
import Flight.Zone (QRadius, Radius(..))
import Zone (MkZone, QLL, describedZones, showQ)
import qualified Distance as D (DistanceClose, toDistanceClose)
import Sphere.Span (spanD, spanR)
import Flight.Earth.Sphere (earthRadius)
import Flight.Geodesy (EarthMath(..), EarthModel(..))
import Flight.Geodesy.Solution (GeodesySolutions(..))
import Flight.Geodesy.Rational ()

units :: TestTree
units =
    testGroup "Meridian arc distance tests"
    [testGroup "With doubles" (uncurry f <$> describedZones)]
    where
        f s  =
            distanceMeridian
                ("Distance between " ++ s ++ " zones on meridian arcs")
                (D.toDistanceClose spanD)
                tolerances

unitsR :: TestTree
unitsR =
    testGroup "Meridian arc distance tests"
    [testGroup "With rationals" (uncurry f <$> describedZones)]
    where
        f s  =
            distanceMeridian
                ("Distance between " ++ s ++ " zones on meridian arcs")
                (D.toDistanceClose spanR)
                tolerances

pts :: (Enum a, Real a, Fractional a) => [(QLL a, QLL a)]
pts =
    meridianArc . convert
    <$> [ x *: [u| 1 deg |] | x <- [5, 10 .. 90]]
    where
        meridianArc d =
            (([u| 0 rad |], [u| 0 rad |]), (d, [u| 0 rad |]))

distances :: (Real a, Fractional a) => [QRadius a [u| m |]]
distances =
    Radius . fromRational' <$>
    zipWith (*:) [1 .. 9] (repeat d)
    where
        x = LatLng (Lat [u| 0 rad |], Lng [u| 0 rad |])
        y = LatLng (Lat $ convert [u| 5 deg |], Lng [u| 0 rad |])
        TaskDistance d = arcLength @Rational @Rational (Haversines, EarthAsSphere earthRadius, defEps) x y

tolerances :: (Real a, Fractional a) => [Quantity a [u| mm |]]
tolerances = repeat [u| 0.19 mm |]

distanceMeridian
    :: (Enum a, Real a, Fractional a)
    => String
    -> D.DistanceClose a
    -> [Quantity a [u| mm |]]
    -> MkZone a a
    -> TestTree
distanceMeridian s f tolerances' g =
    testGroup s
    $ zipWith3
        (\tolerance r@(Radius r') (x, y) ->
                f
                tolerance
                r'
                (showQ x ++ " " ++ showQ y)
                (g r x, g r y))
        tolerances'
        distances
        pts
