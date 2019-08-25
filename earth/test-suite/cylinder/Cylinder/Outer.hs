{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cylinder.Outer
    ( pts
    , ptsUTM
    , distances
    , searchRanges
    , outerCheck
    ) where

import Prelude hiding (span)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure ((*:), (+:), u, convert, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..))
import Flight.Units.Angle (Angle(..))
import Flight.LatLng (QLat, Lat(..), QLng, Lng(..), LatLng(..))
import Flight.Distance (SpanLatLng)
import Flight.Zone (QBearing, Bearing(..), QRadius, Radius(..), Zone(..), ArcSweep(..))
import Flight.Zone.Cylinder (SampleParams(..), Tolerance(..), CircumSample)
import Zone (QLL, showQ)
import Cylinder.Sphere.Span (ZonePointFilter)

pts :: (Enum a, Real a, Fractional a) => [QLL a]
pts =
    f
    <$>
    [ (x *: [u| 1 deg |], y *: [u| 1 deg |])
    | x <- [-90, -45 .. 90]
    , y <- [-180, -90 .. 180]
    ]
    where
        f (x, y) =
            (convert x, convert y)

-- | UTM doesn't cover all latitudes, -80°S .. 84°N.
ptsUTM :: (Enum a, Real a, Fractional a) => [QLL a]
ptsUTM =
    f
    <$>
    [ (x *: [u| 1 deg |], y *: [u| 1 deg |])
    | x <- (negate 79) : [-70, -60 .. 70] ++ [79]
    , y <- [-180, -90 .. 180]
    ]
    where
        f (x, y) =
            (convert x, convert y)

distances :: (Real a, Fractional a) => [QRadius a [u| m |]]
distances =
    Radius . fromRational'
    <$>
    [ [u| 40 m |]
    , [u| 400 m |]
    , convert [u| 1 km |]
    , convert [u| 10 km |]
    , convert [u| 100 km |]
    ]

searchRanges :: (Real a, Fractional a) => [Quantity a [u| m |]]
searchRanges =
    fromRational'
    <$>
    [ convert [u| 1 mm |]
    , convert [u| 1 mm |]
    , convert [u| 10 mm |]
    , convert [u| 100 mm |]
    , [u| 100 m |]
    ]

outerCheck
    ::
        ( Eq a, Show a, Real a, Fractional a
        , Show (QLat a [u| rad |])
        , Show (QLng a [u| rad |])
        )
    => SpanLatLng a
    -> CircumSample a
    -> SampleParams a
    -> QBearing a [u| rad |]
    -> Tolerance a
    -> Quantity a [u| m |]
    -> ZonePointFilter a
    -> QRadius a [u| m |]
    -> LatLng a [u| rad |]
    -> TestTree
outerCheck
    span cs sampleParams br@(Bearing b)
    (Tolerance tolerance)
    sr@(MkQuantity searchRange)
    zpf r@(Radius radius) ll =
    testGroup ("From origin " ++ showQ (lat, lng) ++ " bearing " ++ show b')
    [ HU.testCase
        msg
        $ zpf
            span
            (>)
            ll
            (convert radius +: convert tolerance')
            (fst $ cs sp (ArcSweep br) Nothing Nothing cyl)
        @?= []
    ]
    where
        LatLng (Lat lat, Lng lng) = ll

        msg =
            "No points > "
            ++ show tol
            ++ " outside a "
            ++ show r
            ++ " cylinder when searching within "
            ++ show sr'

        cyl = Cylinder r ll
        sp = sampleParams { spTolerance = Tolerance searchRange }

        tolerance' :: Quantity _ [u| mm |]
        tolerance' = MkQuantity tolerance

        tol :: Quantity Double [u| mm |]
        tol = MkQuantity . realToFrac $ tolerance

        sr' :: Quantity Double [u| m |]
        sr' = fromRational' . toRational' $ sr

        b' :: DMS
        b' = fromQuantity . fromRational' . toRational' $ b
