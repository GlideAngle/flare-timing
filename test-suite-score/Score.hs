module Main (main) where

import Flight.Score (launchValidity)

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "Launch validity is in the range of [0, 1]" $
        \x y -> let lv = launchValidity (SC.getPositive x) (SC.getPositive y)
                in lv >= 0 % 1 && lv <= 1 % 1
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Launch validity is in the range of [0, 1]" $
        \x y -> let lv = launchValidity (QC.getPositive x) (QC.getPositive y)
                in lv >= 0 % 1 && lv <= 1 % 1
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ HU.testCase "Launch validity 0 0 == 0, (nominal actual)" $
        let zero = 0 % 1
        in launchValidity zero zero @?= zero

    , HU.testCase "Launch validity 1 0 == 0, (nominal actual)" $
        let zero = 0 % 1
            one = 1 % 1
        in launchValidity one zero @?= zero

    , HU.testCase "Launch validity 0 1 == 0, (nominal actual)" $
        let zero = 0 % 1
            one = 1 % 1
        in launchValidity zero one @?= zero

    , HU.testCase "Launch validity 1 1 == 1, (nominal actual)" $
        let one = 1 % 1
        in launchValidity one one @?= one
    ]
