module Distance
    ( DistanceEqual
    , DistanceClose
    , toDistanceEqual
    , toDistanceClose
    ) where

import Prelude hiding (span)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.HUnit.Compare ((@?<=))
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.Zone (Zone(..))
import Flight.Zone.Path (distancePointToPoint)
import Tolerance (diff, showTolerance)

type DistanceEqual a
    = Quantity a [u| m |]
    -> String
    -> (Zone a, Zone a)
    -> TestTree

type DistanceClose a
    = Quantity a [u| mm |]
    -> Quantity a [u| m |]
    -> String
    -> (Zone a, Zone a)
    -> TestTree

toDistanceEqual :: (Real a, Fractional a) => SpanLatLng a -> DistanceEqual a
toDistanceEqual span expected title xy =
    testGroup title (f xy)
    where
        f (x, y) =
            [ testCase
                (show (TaskDistance expected))
                $ found @?= TaskDistance expected
            ]
            where
                found = edgesSum $ distancePointToPoint span [x, y]

toDistanceClose :: (Real a, Fractional a) => SpanLatLng a -> DistanceClose a
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
                @?<= TaskDistance tolerance'
            ]
            where
                found = edgesSum $ distancePointToPoint span [x, y]

        tolerance' = convert tolerance
