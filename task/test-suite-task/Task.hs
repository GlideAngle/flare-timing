module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import Zone
import EdgeToEdge
import Crossing

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
        , crossingUnits
        ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "HaversineF distances, are not negative" distanceHaversineF
    , SC.testProperty "Haversine distances, are not negative" distanceHaversine
    , SC.testProperty "Zone distances, point-to-point, are not negative" distancePoint
    , SC.testProperty "Zone distances, point-to-point, are not negative" distancePoint

    -- WARNING: Failing test
    -- there exists ZonesTest [] such that
    -- condition is false
    , SC.testProperty "Zone distances, edge-to-edge, are not negative" distanceEdge
    , SC.testProperty "Zone distances, edge-to-edge are less or equal to point-to-point" distanceLess
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "HaversineF distances, are not negative" distanceHaversineF
    , QC.testProperty "Haversine distances, are not negative" distanceHaversine
    , QC.testProperty "Zone distances, point-to-point, are not negative" distancePoint

    -- WARNING: Failing test
    -- *** Failed! Falsifiable (after 1 test):
    -- ZonesTest [Cylinder RawRadius [u| 4858900382869 % 4950993925855 m |] (0.0 rad, 0.0 rad),Cylinder RawRadius [u| 7377242202805 % 1105125749454 m |] (0.0 rad, 0.0 rad),Cylinder RawRadius [u| 531803483648 % 3572008096645 m |] (0.0 rad, 0.0 rad),Conical i = 7.61 RawRadius [u| 1243886831935 % 5478073641708 m |] (0.0 rad, 0.0 rad)]
    -- Use --quickcheck-replay=324283 to reproduce.
    , QC.testProperty "Zone distances, edge-to-edge, are not negative" distanceEdge
    , QC.testProperty "Zone distances, edge-to-edge are less or equal to point-to-point" distanceLess
    ]
