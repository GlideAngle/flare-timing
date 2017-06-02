module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import Zone

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
        [ units
        , properties
        ]

properties :: TestTree
properties = testGroup "Properties"
        [ scProps
        , qcProps
        ]

units :: TestTree
units = testGroup "Units"
        [ zoneUnits
        ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "Zone distances, point-to-point, are not negative" distancePointToPoint
    , SC.testProperty "Zone distances, edge-to-edge, are not negative" distanceEdgeToEdge
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Zone distances, point-to-point, are not negative" distancePointToPoint
    , QC.testProperty "Zone distances, edge-to-edge, are not negative" distanceEdgeToEdge
    ]
