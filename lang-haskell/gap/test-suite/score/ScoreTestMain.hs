module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import Weighting
import ArrivalFraction
import SpeedFraction
import LinearFraction
import Points

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
        [ weightingUnits
        , arrivalFractionUnits
        , speedFractionUnits
        , linearFractionUnits
        ]

pointUnits :: TestTree
pointUnits =
    testGroup
        "Points"
        [ tallyUnits
        ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "Distance weight is in the range of [0, 1]" distanceWeight
    , SC.testProperty "Arrival weight is in the range of [0, 1]" arrivalWeight
    , SC.testProperty "Leading weight is in the range of [0, 1]" leadingWeight
    , SC.testProperty "Time weight is in the range of [0, 1]" timeWeight
    , SC.testProperty "Arrival fraction is in the range of [0.2, 1]" arrivalFraction
    , SC.testProperty "Speed fraction pilot time is not less than best time" speedFractionInputs
    , SC.testProperty "Speed fraction is in the range of [0, 1]" speedFraction
    , SC.testProperty "Linear distance fraction is in the range of [0, 1]" linearFraction
    , SC.testProperty "Task points add up with Hg penalties" taskPointsHg
    , SC.testProperty "Task points add up with Pg penalties" taskPointsPg
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Distance weight is in the range of [0, 1]" distanceWeight
    , QC.testProperty "Arrival weight is in the range of [0, 1]" arrivalWeight
    , QC.testProperty "Leading weight is in the range of [0, 1]" leadingWeight
    , QC.testProperty "Time weight is in the range of [0, 1]" timeWeight
    , QC.testProperty "Arrival fraction is in the range of [0.2, 1]" arrivalFraction
    , QC.testProperty "Speed fraction pilot time is not less than best time" speedFractionInputs
    , QC.testProperty "Speed fraction is in the range of [0, 1]" speedFraction
    , QC.testProperty "Linear distance fraction is in the range of [0, 1]" linearFraction
    , QC.testProperty "Task points add up with Hg penalties" taskPointsHg
    , QC.testProperty "Task points add up with Pg penalties" taskPointsPg
    ]
