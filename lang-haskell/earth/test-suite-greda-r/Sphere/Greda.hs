module Sphere.Greda (unitsR) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)

import Flight.Units ()
import Flight.Zone (Zone(..))
import ToLatLng (toLatLngR)
import qualified Greda as G (task1, task2, task3)
import Sphere.Span (sepR)

task1, task2, task3 :: [Zone Rational]
task1 = G.task1 toLatLngR
task2 = G.task2 toLatLngR
task3 = G.task3 toLatLngR

unitsR :: TestTree
unitsR =
    testGroup "Greda 2018 zones are separated"
    [ HU.testCase "Task 1" $ sepR task1 @?= True
    , HU.testCase "Task 2" $ sepR task2 @?= True
    , HU.testCase "Task 3" $ sepR task3 @?= True
    ]
