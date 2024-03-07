{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cylinder.Flat.Span
    ( ZonePointFilter
    , spanD, sepD, csD, spD
    , spanR, sepR, csR, spR
    , zpFilter
    ) where

import Prelude hiding (span)
import Data.Ratio((%))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (LatLng(..))
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.Zone (Zone(..))
import Flight.Zone.Path (distancePointToPoint)
import Flight.Zone.Cylinder
    (Samples(..), SampleParams(..), Tolerance(..), CircumSample, ZonePoint(..))
import Flight.Geodesy (EarthModel(..), EarthMath(..), Projection(..))
import Flight.Geodesy.Solution (GeoZones(..))
import Flight.Geodesy.Double ()
import Flight.Geodesy.Rational ()
import Flat.Span (spanD, spanR, sepD, sepR)

eps :: Epsilon
eps = Epsilon $ 1 % 1000000000000000000

mm30 :: Fractional a => Tolerance a
mm30 = Tolerance . fromRational $ 30 % 1000

csD :: CircumSample Double
csD = circumSample @Double @Double (Pythagorus, EarthAsFlat UTM)

csR :: CircumSample Rational
csR = circumSample @Rational @Rational (Pythagorus, EarthAsFlat UTM, eps)

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
