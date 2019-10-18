module Flat.Flat (units) where

import Test.Tasty (TestTree, testGroup)

import Flat.Coincident (coincidentUnits, touchingUnits, disjointUnits)

units :: TestTree
units =
    testGroup
    "On a flat Earth (UTM Projection) using Pythagoras' theorem a² + b² = c²"
    [ coincidentUnits
    , touchingUnits
    , disjointUnits
    ]
