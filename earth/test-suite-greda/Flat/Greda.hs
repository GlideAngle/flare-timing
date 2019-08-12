module Flat.Greda (units) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)

import Flight.Units ()
import Flight.Zone (Zone(..))
import ToLatLng (toLatLngD)
import qualified Greda as G (task1, task2, task3)
import Flat.Span (sepD)

task1, task2, task3 :: [Zone Double]
task1 = G.task1 toLatLngD
task2 = G.task2 toLatLngD
task3 = G.task3 toLatLngD

units :: TestTree
units =
    testGroup "Greda 2018 zones are separated"
    [ HU.testCase "Task 1" $ sepD task1 @?= True
    , HU.testCase "Task 2" $ sepD task2 @?= True
    , HU.testCase "Task 3" $ sepD task3 @?= True
    ]
