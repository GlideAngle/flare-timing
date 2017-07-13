module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import Data.Number.RoundingFunctions (dpRound, sdRound)

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
        [ roundUnits
        ]

roundUnits :: TestTree
roundUnits = testGroup "Rounding ..."
    [ testGroup "Rounding to 2 decimal places"
        [ HU.testCase "123456789.0 => no change" $
            dpRound 2 (123456789.0 :: Rational) @?= 123456789.0
        , HU.testCase "1234.56789 => 1234.57" $
            dpRound 2 (1234.56789 :: Rational) @?= 1234.57
        , HU.testCase "123.456789 => 123.46" $
            dpRound 2 (123.456789 :: Rational) @?= 123.46
        , HU.testCase "12.3456789 => 12.35" $
            dpRound 2 (12.3456789 :: Rational) @?= 12.35
        , HU.testCase "1.23456789 => 1.23" $
            dpRound 2 (1.23456789 :: Rational) @?= 1.23
        , HU.testCase "0.123456789 => 0.12" $
            dpRound 2 (0.123456789 :: Rational) @?= 0.12
        , HU.testCase "0.0123456789 => 0.01" $
            dpRound 2 (0.0123456789 :: Rational) @?= 0.01
        , HU.testCase "0.0000123456789 => 0.0" $
            dpRound 2 (0.0000123456789 :: Rational) @?= 0.0
        ]
    , testGroup "Rounding 4 significant digits"
        [ HU.testCase "123456789.0 => 123500000.0" $
            sdRound 4 (123456789.0 :: Rational) @?= 123500000.0
        , HU.testCase "1234.56789 => 1235.0" $
            sdRound 4 (1234.56789 :: Rational) @?= 1235.0
        , HU.testCase "123.456789 => 123.5" $
            sdRound 4 (123.456789 :: Rational) @?= 123.5
        , HU.testCase "12.3456789 => 12.35" $
            sdRound 4 (12.3456789 :: Rational) @?= 12.35
        , HU.testCase "1.23456789 => 1.235" $
            sdRound 4 (1.23456789 :: Rational) @?= 1.235
        , HU.testCase "0.123456789 => 0.1235" $
            sdRound 4 (0.123456789 :: Rational) @?= 0.1235
        , HU.testCase "0.0123456789 => 0.01235" $
            sdRound 4 (0.0123456789 :: Rational) @?= 0.01235
        , HU.testCase "0.0000123456789 => 0.00001235" $
            sdRound 4 (0.0000123456789 :: Rational) @?= 0.00001235
        ]
    ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "Rounding to zero or fewer decimal places" scDpZero
    , SC.testProperty "Rounding is idempotent" dpIdempotent
    , SC.testProperty "Zero or fewer significant digits" scSdZero
    ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "Rounding to zero or fewer decimal places" qcDpZero
    , QC.testProperty "Rounding is idempotent" dpIdempotent
    , QC.testProperty "Zero or fewer significant digits" qcSdZero
    ]

scDpZero :: Monad m => Integer -> Rational -> SC.Property m
scDpZero dp x =
    dp < 0 SC.==> dpRound dp x == x

scSdZero :: Monad m => Integer -> Rational -> SC.Property m
scSdZero dp x =
    dp < 0 SC.==> sdRound dp x == x

qcDpZero :: Integer -> Rational -> QC.Property
qcDpZero dp x =
    dp < 0 QC.==> dpRound dp x == x

qcSdZero :: Integer -> Rational -> QC.Property
qcSdZero dp x =
    dp < 0 QC.==> sdRound dp x == x

dpIdempotent :: Integer -> Rational -> Bool
dpIdempotent dp x =
    let y = dpRound dp x in dpRound dp y == y
