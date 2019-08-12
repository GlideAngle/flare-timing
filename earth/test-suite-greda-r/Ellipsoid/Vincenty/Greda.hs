module Ellipsoid.Vincenty.Greda (unitsR) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)

import Flight.Units ()
import Flight.Zone (Zone(..))
import ToLatLng (toLatLngR)
import Flight.Earth.Ellipsoid (wgs84)
import qualified Greda as G (task1, task2, task3)
import qualified Ellipsoid.Vincenty.Span as E (sepR)

task1, task2, task3 :: [Zone Rational]
task1 = G.task1 toLatLngR
task2 = G.task2 toLatLngR
task3 = G.task3 toLatLngR

sep :: [Zone Rational] -> Bool
sep = E.sepR wgs84

unitsR :: TestTree
unitsR =
    testGroup "Greda 2018 zones are separated"
    [ HU.testCase "Task 1" $ sep task1 @?= True
    , HU.testCase "Task 2" $ sep task2 @?= True
    , HU.testCase "Task 3" $ sep task3 @?= True
    ]
