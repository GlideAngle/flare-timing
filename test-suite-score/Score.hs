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
    -- WARNING: Failing test.
    --    there exist 0 1 0 1 such that
    --      condition is false
    [ SC.testProperty "Launch validity is in the range of [0, 1]" scLaunchValidity
    ]


qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    -- WARNING: Failing test.
    --    *** Failed! Falsifiable (after 1 test and 1 shrink):
    --    NonNegative {getNonNegative = 0}
    --    Positive {getPositive = 1}
    --    NonNegative {getNonNegative = 0}
    --    Positive {getPositive = 1}
    --    Use --quickcheck-replay '0 TFGenR 0000002DEC6F478500000000002625A0000000000000E2220000000000000000 0 1 1 0' to reproduce.
    [ QC.testProperty "Launch validity is in the range of [0, 1]" qcLaunchValidity
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    -- WARNING: Failing test.
    --  expected: 0 % 1
    --   but got: 2751699372323373 % 562949953421312
    [ HU.testCase "Launch validity 0 0 == 0, (nominal actual)" $
        launchValidity (0 % 1) (0 % 1) @?= (0 % 1)

    , HU.testCase "Launch validity 1 0 == 0, (nominal actual)" $
        launchValidity (1 % 1) (0 % 1) @?= (0 % 1)

    -- WARNING: Failing test.
    --  expected: 0 % 1
    --   but got: 2751699372323373 % 562949953421312
    , HU.testCase "Launch validity 0 1 == 0, (nominal actual)" $
        launchValidity (0 % 1) (1 % 1) @?= (0 % 1)

    -- WARNING: Failing test.
    --  expected: 1 % 1
    --   but got: 2751699372323373 % 562949953421312
    , HU.testCase "Launch validity 1 1 == 1, (nominal actual)" $
        launchValidity (1 % 1) (1 % 1) @?= (1 % 1)
    ]

scLaunchValidity
    :: Monad m => SC.NonNegative Integer
    -> SC.Positive Integer
    -> SC.NonNegative Integer
    -> SC.Positive Integer
    -> SC.Property m
scLaunchValidity
    (SC.NonNegative nx)
    (SC.Positive dx)
    (SC.NonNegative ny)
    (SC.Positive dy) =
    nx <= dx && ny <= dy SC.==>
    let nominalLaunch = nx % dx
        fractionLaunching = ny % dy
        lv = launchValidity nominalLaunch fractionLaunching
    in lv >= (0 % 1) && lv <= (1 % 1)

qcLaunchValidity
    :: QC.NonNegative Integer
    -> QC.Positive Integer
    -> QC.NonNegative Integer
    -> QC.Positive Integer
    -> QC.Property
qcLaunchValidity
    (QC.NonNegative nx)
    (QC.Positive dx)
    (QC.NonNegative ny)
    (QC.Positive dy) =
    nx <= dx && ny <= dy QC.==>
    let nominalLaunch = nx % dx
        fractionLaunching = ny % dy
        lv = launchValidity nominalLaunch fractionLaunching
    in lv >= (0 % 1) && lv <= (1 % 1)
