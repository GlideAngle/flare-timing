{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cylinder.Inner
    ( pts
    , ptsUTM
    , distances
    , searchRanges
    , tolerances
    , innerCheck
    ) where

import Prelude hiding (span)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure ((*:), (-:), u, convert, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (QLat, Lat(..), QLng, Lng(..), LatLng(..))
import Flight.Distance (SpanLatLng)
import Flight.Zone (QBearing, QRadius, Radius(..), Zone(..), ArcSweep(..))
import Flight.Zone.Cylinder (SampleParams(..), Tolerance(..), CircumSample)
import Zone (QLL, showQ)
import Cylinder.Sphere.Span (ZonePointFilter)

pts :: (Enum a, Real a, Fractional a) => [QLL a]
pts =
    f
    <$>
    [ ((x - 90) *: [u| 1 deg |], (y - 180) *: [u| 1 deg |])
    | x <- [0, 45 .. 180]
    , y <- [0, 90 .. 360]
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
    | x <- [-80.0, -72.0 .. 80.0]
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

tolerances :: (Real a, Fractional a) => [Quantity a [u| mm |]]
tolerances =
    fromRational' <$>
    [ [u| 1 mm |]
    , [u| 1 mm |]
    , [u| 10 mm |]
    , [u| 100 mm |]
    , convert [u| 100 m |]
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

innerCheck
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
innerCheck
    span cs sampleParams br
    (Tolerance tolerance)
    sr@(MkQuantity searchRange)
    zpf r@(Radius radius) ll =
    testGroup ("At " ++ showQ (lat, lng))
    [ HU.testCase
        msg
        $ zpf
            span
            (<)
            ll
            (convert radius -: convert tolerance')
            (fst $ cs sp (ArcSweep br) Nothing Nothing cyl)
        @?= []
    ]
    where
        LatLng (Lat lat, Lng lng) = ll

        msg =
            "No points > "
            ++ show tol
            ++ " inside a "
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
