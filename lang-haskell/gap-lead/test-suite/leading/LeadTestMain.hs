module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import LeadingCoefficient

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ properties
        ]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "A cleaned track is smaller if there is some flying away from goal" cleanTrack
    , SC.testProperty "Leading fraction is in the range of [0, 1]" leadingFractions
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "A cleaned track is smaller if there is some flying away from goal" cleanTrack
    , QC.testProperty "Leading fraction is in the range of [0, 1]" leadingFractions
    ]
