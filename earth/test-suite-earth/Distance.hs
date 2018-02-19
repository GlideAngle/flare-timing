{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Distance where

import Prelude hiding (span)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.HUnit.Compare ((@?<=))
import Data.UnitsOfMeasure (u, convert, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.Zone (Zone(..))
import Flight.Zone.Path (distancePointToPoint)
import Tolerance (diff, showTolerance)

toDistanceEqual
    :: SpanLatLng Rational
    -> Quantity Rational [u| m |]
    -> String
    -> (Zone Rational, Zone Rational)
    -> TestTree
toDistanceEqual span expected title xy =
    testGroup title (f xy)
    where
        f (x, y) =
            [ testCase
                (show (TaskDistance expected))
                $ found @?= (TaskDistance expected)
            ]
            where
                found = edgesSum $ distancePointToPoint span [x, y]

toDistanceClose
    :: SpanLatLng Rational
    -> Quantity Rational [u| mm |]
    -> Quantity Rational [u| m |]
    -> String
    -> (Zone Rational, Zone Rational)
    -> TestTree
toDistanceClose span tolerance expected title xy =
    testGroup title (f xy)
    where
        f (x, y) =
            [ testCase
                ( "within ± "
                ++ showTolerance tolerance'
                ++ " of expected "
                ++ show (TaskDistance expected)
                )
                $ diff found (TaskDistance expected)
                @?<= (TaskDistance . toRational' $ tolerance')
            ]
            where
                found = edgesSum $ distancePointToPoint span [x, y]

        tolerance' = convert tolerance
