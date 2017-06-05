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
    -- WARNING: Failing test.
    --        *** Failed! Falsifiable (after 1 test):
    --        ZonesTest [Cylinder (Radius (947018672917 % 356822369800)) (LatLng (0 % 1,0 % 1)),Cylinder (Radius (1511173171283 % 3812845238761)) (LatLng (0 % 1,0 % 1)),Conical (Incline (0 % 1)) (Radius (2586691890764 % 3837004734663)) (LatLng (0 % 1,0 % 1)),Cylinder (Radius (184730680699 % 4424891088552)) (LatLng (0 % 1,0 % 1)),SemiCircle (Radius (4314968157782 % 922757368359)) (LatLng (0 % 1,0 % 1)),Cylinder (Radius (9791138992243 % 9884230385346)) (LatLng (0 % 1,0 % 1)),Cylinder (Radius (928961456620 % 2511702855613)) (LatLng (0 % 1,0 % 1))]
    --        Use --quickcheck-replay '0 TFGenR 0000003D5B9F183100000000004C4B40000000000000E2350003C94B422B5C00 0 1 1 0' to reproduce.
    [ QC.testProperty "Zone distances, point-to-point, are not negative" distancePointToPoint
    , QC.testProperty "Zone distances, edge-to-edge, are not negative" distanceEdgeToEdge

    -- WARNING: Failing test.
    --        *** Failed! Falsifiable (after 1 test):
    --        ZonesTest [SemiCircle (Radius (3873597955179 % 1776524623787)) (LatLng (0 % 1,0 % 1)),Vector (Bearing (1468010718739 % 1436033836632)) (LatLng (0 % 1,0 % 1)),Vector (Bearing (4244134843206 % 8855629438511)) (LatLng (0 % 1,0 % 1)),Cylinder (Radius (2786132183137 % 1369755534631)) (LatLng (0 % 1,0 % 1)),SemiCircle (Radius (8218669047391 % 8624143127058)) (LatLng (0 % 1,0 % 1)),Cylinder (Radius (4977706473871 % 690668771128)) (LatLng (0 % 1,0 % 1)),Cylinder (Radius (9515108365273 % 3927980572893)) (LatLng (0 % 1,0 % 1))]
    --        Use --quickcheck-replay '0 TFGenR 0000003D5B9F183100000000004C4B40000000000000E2350003C94B422B5C00 0 4 3 0' to reproduce.
    , QC.testProperty "Zone distances, edge-to-edge are less or equal to point-to-point" distanceLess
    ]
