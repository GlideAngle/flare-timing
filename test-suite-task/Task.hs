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
    [ SC.testProperty "Zone distances, point-to-point, are not negative" distancePointToPoint
    , SC.testProperty "Zone distances, edge-to-edge, are not negative" distanceEdgeToEdge
    , SC.testProperty "Zone distances, edge-to-edge are less or equal to point-to-point" distanceLess
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Zone distances, point-to-point, are not negative" distancePointToPoint

    -- WARNING: Failing test.
--        *** Failed! Falsifiable (after 1 test):
--        ZonesTest [Cylinder (Radius (1495751350164 % 1810180075861)) (LatLng (0 % 1,0 % 1)),Cylinder (Radius (2530834715266 % 1473529153461)) (LatLng (0 % 1,0 % 1)),Cylinder (Radius (3149546472136 % 385110902919)) (LatLng (0 % 1,0 % 1))]
--        Use --quickcheck-replay '0 TFGenR 00000085AEFF7CA50000000000989680000000000000E2350004D90169CBC800 0 2 2 0' to reproduce.
    , QC.testProperty "Zone distances, edge-to-edge, are not negative" distanceEdgeToEdge

    -- WARNING: Failing test.
--        *** Failed! Falsifiable (after 1 test):
--        ZonesTest [Cylinder (Radius (3431954750077 % 4838809452999)) (LatLng (0 % 1,0 % 1)),SemiCircle (Radius (8673959593932 % 6093521227033)) (LatLng (0 % 1,0 % 1)),Cylinder (Radius (6204125052028 % 8452800444999)) (LatLng (0 % 1,0 % 1)),Cylinder (Radius (2463177115345 % 7909999292238)) (LatLng (0 % 1,0 % 1)),Point (LatLng (0 % 1,0 % 1)),Cylinder (Radius ( 3607924760267 % 2236736149380)) (LatLng (0 % 1,0 % 1))]
--        Use --quickcheck-replay '0 TFGenR 00000085AEFF7CA50000000000989680000000000000E2350004D90169CBC800 0 4 3 0' to reproduce.
    , QC.testProperty "Zone distances, edge-to-edge are less or equal to point-to-point" distanceLess
    ]
