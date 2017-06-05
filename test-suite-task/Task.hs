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
    , QC.testProperty "Zone distances, edge-to-edge, are not negative" distanceEdge

    -- WARNING: Failing test.
    --        *** Failed! Falsifiable (after 2 tests):
    ----        ZonesTest [SemiCircle (Radius (1957727751263 % 2686460680370)) (LatLng (4463150117677 % 6427999969522,(-2542508014219) % 7797777489448)),Vector (Bearing (198155016790 % 243073218009)) (LatLng (1141795966181 % 104150071814,(-468010398311) % 299473358221))]
    --        Use --quickcheck-replay '1 TFGenR 0000008836940B5D0000000000989680000000000000E2350005F334657B9480 0 12 4 0' to reproduce.
    , QC.testProperty "Zone distances, edge-to-edge are less or equal to point-to-point" distanceLess
    ]
