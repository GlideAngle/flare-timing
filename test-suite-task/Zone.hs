module Zone (zoneUnits, distance) where

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
    [ HU.testCase "No zones = zero distance" $
        FS.distance [] @?= TaskDistance 0
    ]

correct :: [Zone] -> TaskDistance -> Bool
correct _ _ = False

distance :: ZonesTest -> Bool
distance (ZonesTest xs) =
    correct xs $ FS.distance xs
