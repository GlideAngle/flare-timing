{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cylinder.Ellipsoid.Vincenty.Span
    ( ZonePointFilter
    , spanD, csD, spD
    , spanR, csR, spR
    , zpFilter
    ) where

import Prelude hiding (span)
import Data.Ratio((%))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (LatLng(..))
import Flight.LatLng.Rational (defEps)
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.Zone (Zone(..))
import Flight.Zone.Path (distancePointToPoint)
import Flight.Zone.Cylinder
    (Samples(..), SampleParams(..), Tolerance(..), CircumSample, ZonePoint(..))
import Flight.Earth.Ellipsoid (wgs84)
import Flight.Geodesy (EarthModel(..), EarthMath(..))
import Flight.Geodesy.Solution (GeodesySolutions(..), GeoZones(..))
import Flight.Geodesy.Double ()
import Flight.Geodesy.Rational ()

mm30 :: Fractional a => Tolerance a
mm30 = Tolerance . fromRational $ 30 % 1000

spanD :: SpanLatLng Double
spanD = arcLength @Double @Double (Vincenty, EarthAsEllipsoid wgs84)

spanR :: SpanLatLng Rational
spanR = arcLength @Rational @Rational (Vincenty, EarthAsEllipsoid wgs84, defEps)

csD :: CircumSample Double
csD = circumSample @Double @Double (Vincenty, EarthAsEllipsoid wgs84)

csR :: CircumSample Rational
csR = circumSample @Rational @Rational (Vincenty, EarthAsEllipsoid wgs84, defEps)

spD :: SampleParams Double
spD =
    SampleParams
        { spSamples = [Samples 100]
        , spTolerance = mm30
        }

spR :: SampleParams Rational
spR =
    SampleParams
        { spSamples = [Samples 100]
        , spTolerance = mm30
        }

type ZonePointFilter a
    = SpanLatLng a
    -> (Quantity a [u| m |] -> Quantity a [u| m |] -> Bool)
    -> LatLng a [u| rad |]
    -> Quantity a [u| m |]
    -> [ZonePoint a]
    -> [ZonePoint a]

zpFilter
    :: Real a
    => SpanLatLng a
    -> (Quantity a [u| m |] -> Quantity a [u| m |] -> Bool)
    -> LatLng a [u| rad |]
    -> Quantity a [u| m |]
    -> [ZonePoint a]
    -> [ZonePoint a]
zpFilter span cmp origin d =
    filter (\x -> zpDistance span origin x `cmp` d)

zpDistance
    :: (Eq a, Real a)
    => SpanLatLng a
    -> LatLng a [u| rad |]
    -> ZonePoint a
    -> Quantity a [u| m |]
zpDistance span origin ZonePoint{point} =
    d
    where
        TaskDistance d =
            edgesSum $ distancePointToPoint span [Point origin, Point point]
