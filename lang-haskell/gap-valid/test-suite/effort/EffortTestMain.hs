module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import DifficultyFraction

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [units, properties]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

units :: TestTree
units = testGroup "Difficulty Fraction Units" [difficultyUnits]


scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "Difficulty lookahead is in the range of [30, 30 * best flown] chunks" lookahead
    , SC.testProperty "Difficulty fraction is in the range of [0, 1]" difficulty
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Difficulty lookahead is in the range of [30, 30 * best flown] chunks" lookahead
    -- WARNING: Takes a long time.
    --, QC.testProperty "Difficulty fraction is in the range of [0, 1]" difficulty
    ]
