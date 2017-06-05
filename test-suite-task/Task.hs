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
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Zone distances, point-to-point, are not negative" distancePointToPoint

    -- WARNING: Failing test.
    --        *** Failed! Exception: 'Prelude.head: empty list' (after 4 tests):
    --        ZonesTest [Vector (Bearing ((-8128971062566) % 1890401950547)) (LatLng (17587866865261 % 1776959198642,5128428415813 % 6215300640898)),SemiCircle (Radius (5940919133137 % 3734234476148)) (LatLng (7344651197476 % 9602508952187,19192052829749 % 4604097196246)),Point (LatLng (15366472926857 % 5752281391201,(-11618437853167) % 6393280256761))]
    --        Use --quickcheck-replay '3 TFGenR 00000004CC168D850000000000989680000000000000E2350001B90FF2BF9340 0 30 5 0' to reproduce.
    , QC.testProperty "Zone distances, edge-to-edge, are not negative" distanceEdgeToEdge
    ]
