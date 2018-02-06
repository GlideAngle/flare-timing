module Crossing (crossingUnits) where

import Test.Tasty (TestTree, testGroup)

import Crossing.Day8.Zone1 as D8Z1
import Crossing.Day8.Zone2 as D8Z2
import Crossing.Day8.Zone3 as D8Z3
import Crossing.Day8.Zone4 as D8Z4
import Crossing.Day8.Zone5 as D8Z5

crossingUnits :: TestTree
crossingUnits = testGroup "Zone edge shortest path unit tests"
    [ forbesUnits
    ]

forbesUnits :: TestTree
forbesUnits = testGroup "Forbes 2011/2012 crossings, pilot Lukas Bader"
    [ D8Z1.units
    , D8Z2.units
    , D8Z3.units
    , D8Z4.units
    , D8Z5.units
    ]
