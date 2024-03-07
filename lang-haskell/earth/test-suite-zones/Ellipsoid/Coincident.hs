module Ellipsoid.Coincident (coincidentUnits) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Data.UnitsOfMeasure (u, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Zone (QRadius, Radius(..))
import Flight.Zone.Path (distancePointToPoint)
import Zone (MkZone, QLL, showQ, describedZones)
import qualified Distance as D (DistanceEqual, toDistanceEqual)
import Flight.Earth.Ellipsoid (wgs84)
import Cylinder.Ellipsoid.Vincenty.Span (spanD, spanR)

coincidentUnits :: TestTree
coincidentUnits =
    testGroup "Coincident zones unit tests"
    $ emptyDistance
    :
    [ testGroup "With doubles" (uncurry f <$> describedZones)
    , testGroup "With rationals" (uncurry g <$> describedZones)
    ]
    where
        f s =
            distanceZero
                ("Distance between coincident " ++ s ++ " zones")
                (D.toDistanceEqual $ spanD wgs84)

        g s =
            distanceZero
                ("Distance between coincident " ++ s ++ " zones")
                (D.toDistanceEqual $ spanR wgs84)

emptyDistance :: TestTree
emptyDistance =
    testGroup "Point-to-point distance"
    [ testCase "No zones = zero point-to-point distance" $
        edgesSum (distancePointToPoint (spanR wgs84) [])
        @?= (TaskDistance $ MkQuantity 0)
    ]

pts :: (Enum a, Real a, Fractional a) => [(QLL a, QLL a)]
pts =
    [ ((z, z), (z, z))
    , ((m, z), (m, z))
    , ((z, m), (z, m))
    , ((m, m), (m, m))
    ]
    where
        z = [u| 0 rad |]
        m = convert [u| 45 deg |]

distances :: (Real a, Fractional a) => [QRadius a [u| m |]]
distances =
    repeat . Radius . fromRational' $ [u| 0 m |]

distanceZero
    :: (Enum a, Real a, Fractional a)
    => String
    -> D.DistanceEqual a
    -> MkZone a a
    -> TestTree
distanceZero s f g =
    testGroup s
    $ zipWith
        (\r@(Radius r') (x, y) ->
                f
                r'
                (showQ x ++ " " ++ showQ y)
                (g r x, g r y))
        distances
        pts
