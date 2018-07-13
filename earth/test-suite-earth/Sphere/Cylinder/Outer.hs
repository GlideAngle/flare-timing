{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Sphere.Cylinder.Outer (outerCylinderUnits) where

import Prelude hiding (span)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure
    ((*:), (+:), u, convert, unQuantity, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Distance (SpanLatLng)
import Flight.Zone (Bearing(..), QRadius, Radius(..), Zone(..))
import Flight.Zone.Cylinder (SampleParams(..), Tolerance(..), CircumSample)
import Zone (QLL, showQ)
import Sphere.Cylinder.Span
    ( ZonePointFilter
    , spanD, csD, spD
    , spanR, csR, spR
    , zpFilter
    )

bearingD :: [Bearing Double]
bearingD =
    Bearing <$>
    [ f $ x *: [u| 1 deg |]
    | x <- [0, 45 .. 360]
    ]
    where
        f :: Quantity Double [u| deg |] -> Quantity Double [u| rad |]
        f = convert

bearingR :: [Bearing Rational]
bearingR =
    Bearing . toRational' <$>
    [ f $ x *: [u| 1 deg |]
    | x <- [0, 120]
    ]
    where
        f :: Quantity Double [u| deg |] -> Quantity Double [u| rad |]
        f = convert

ptsD :: (Enum a, Real a, Fractional a) => [QLL a]
ptsD =
    f
    <$>
    [ ((x - 90) *: [u| 1 deg |], (y - 180) *: [u| 1 deg |])
    | x <- [0, 45 .. 180]
    , y <- [0, 90 .. 360]
    ]
    where
        f (x, y) =
            (convert x, convert y)

ptsR :: (Enum a, Real a, Fractional a) => [QLL a]
ptsR =
    f
    <$>
    [ ((x - 90) *: [u| 1 deg |], (y - 180) *: [u| 1 deg |])
    | x <- [0, 90 .. 180]
    , y <- [0, 180 .. 360]
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

tolerancesD :: (Real a, Fractional a) => [Quantity a [u| mm |]]
tolerancesD =
    repeat $ fromRational' [u| 0.0000001 mm |]

tolerancesR :: (Real a, Fractional a) => [Quantity a [u| mm |]]
tolerancesR =
    repeat $ fromRational' [u| 0 mm |]

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

outerCylinderUnits :: TestTree
outerCylinderUnits =
    testGroup "When points meant to be on the boundary are outside a cylinder"
    [ testGroup "With doubles"
        [ outerCheck spanD csD spD b t s zpFilter d p
        | p <- (\(x, y) -> (LatLng (Lat x, Lng y))) <$> ptsD
        , b <- bearingD
        , (d, t, s) <-
            [ (d, t, s)
            | d <- distances
            | t <- Tolerance . unQuantity <$> tolerancesD
            | s <- searchRanges
            ]
        ]

    , testGroup "With rationals"
        [ outerCheck spanR csR spR b t s zpFilter d p
        | p <- (\(x, y) -> (LatLng (Lat x, Lng y))) <$> ptsR
        , b <- bearingR
        , (d, t, s) <-
            [ (d, t, s)
            | d <- distances
            | t <- Tolerance . unQuantity <$> tolerancesR
            | s <- searchRanges
            ]
        ]
    ]

outerCheck
    ::
        ( Eq a, Show a, Real a, Fractional a
        , Show (Lat a [u| rad |])
        , Show (Lng a [u| rad |])
        )
    => SpanLatLng a
    -> CircumSample a
    -> SampleParams a
    -> Bearing a
    -> Tolerance a
    -> Quantity a [u| m |]
    -> ZonePointFilter a
    -> QRadius a [u| m |]
    -> LatLng a [u| rad |]
    -> TestTree
outerCheck
    span cs sampleParams br
    (Tolerance tolerance)
    sr@(MkQuantity searchRange)
    zpf r@(Radius radius) ll =
    testGroup ("From origin " ++ showQ (lat, lng) ++ " bearing " ++ show br)
    [ HU.testCase
        msg
        $ zpf
            span
            (>)
            ll
            (convert radius +: convert tolerance')
            (fst $ cs sp br Nothing cyl)
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
