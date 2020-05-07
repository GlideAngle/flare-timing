module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import ArrivalFraction
import SpeedFraction
import LinearFraction
import Points.Hg
import Points.Pg
import Points.Props

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ units
        , properties
        ]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

units :: TestTree
units = testGroup
            "Units"
            [ fractionUnits
            , pointUnits
            ]

fractionUnits :: TestTree
fractionUnits =
    testGroup
        "Fractions"
        [ arrivalFractionUnits
        , speedFractionUnits
        , linearFractionUnits
        ]

pointUnits :: TestTree
pointUnits =
    testGroup
        "Points"
        [ hgUnits
        , pgUnits
        ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "Arrival fraction is in the range of [0.2, 1]" arrivalFraction
    , SC.testProperty "Speed fraction pilot time is not less than best time" speedFractionInputs
    , SC.testProperty "Speed fraction is in the range of [0, 1]" speedFraction
    , SC.testProperty "Linear distance fraction is in the range of [0, 1]" linearFraction
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Arrival fraction is in the range of [0.2, 1]" arrivalFraction
    , QC.testProperty "Speed fraction pilot time is not less than best time" speedFractionInputs
    , QC.testProperty "Speed fraction is in the range of [0, 1]" speedFraction
    , QC.testProperty "Linear distance fraction is in the range of [0, 1]" linearFraction
    {- TODO: Failing test - memory leak?
    , QC.testProperty "Task points add up with Hg penalties" hgTaskPoints
    -}
    , QC.testProperty "Task points add up with Pg penalties" pgTaskPoints
    ]
