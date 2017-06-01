module Zone (zoneUnits, distancePointToPoint, distanceEdgeToEdge) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)

import qualified Flight.Task as FS
import Flight.Task
    ( Zone(..)
    , TaskDistance(..)
    )

import TestNewtypes

zoneUnits :: TestTree
zoneUnits = testGroup "Add up task distance"
    [ HU.testCase "No zones = zero point-to-point distance" $
        FS.distancePointToPoint [] @?= TaskDistance 0

    , HU.testCase "No zones = zero edge-to-edge distance" $
        FS.distanceEdgeToEdge [] @?= TaskDistance 0
    ]

correct :: [Zone] -> TaskDistance -> Bool
correct _ _ = False

distancePointToPoint :: ZonesTest -> Bool
distancePointToPoint (ZonesTest xs) =
    correct xs $ FS.distancePointToPoint xs

distanceEdgeToEdge :: ZonesTest -> Bool
distanceEdgeToEdge (ZonesTest xs) =
    correct xs $ FS.distancePointToPoint xs
