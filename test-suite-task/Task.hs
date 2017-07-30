module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import Zone
import EdgeToEdge

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
        , edgeToEdgeUnits
        ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "Zone distances, point-to-point, are not negative" distancePoint
    , SC.testProperty "Zone distances, edge-to-edge, are not negative" distanceEdge
    , SC.testProperty "Zone distances, edge-to-edge are less or equal to point-to-point" distanceLess
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Zone distances, point-to-point, are not negative" distancePoint
    -- WARNING: Failed test.
    -- *** Failed! Falsifiable (after 1 test):
    -- ZonesTest [Cylinder r = 1.581 (0.0 rad, 0.0 rad),Cylinder r = 0.84 (0.0 rad, 0.0 rad)]
    -- Use --quickcheck-replay '0 TFGenR 00000010D1A1217D00000000000F4240000000000000E26C000009FED00F3240 0 1 1 0' to reproduce.
    , QC.testProperty "Zone distances, edge-to-edge, are not negative" distanceEdge
    , QC.testProperty "Zone distances, edge-to-edge are less or equal to point-to-point" distanceLess
    ]
