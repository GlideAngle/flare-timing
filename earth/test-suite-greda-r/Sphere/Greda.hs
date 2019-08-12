module Sphere.Greda (units) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)

import Flight.Units ()
import Flight.Zone (Zone(..))
import ToLatLng (toLatLngR)
import qualified Greda as G (task1)
import Sphere.Span (sepR)

task1 :: [Zone Rational]
task1 = G.task1 toLatLngR

units :: TestTree
units =
    testGroup "Greda 2011/2012"
    [ HU.testCase "Task 1 zones are separated" $
        sepR task1 @?= True
    ]
