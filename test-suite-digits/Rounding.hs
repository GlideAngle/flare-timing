module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit as HU ((@?=), testCase)

import Data.Number.RoundingFunctions (dpRound, sdRound)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
        [ units
        ]

units :: TestTree
units = testGroup "Units"
        [ roundUnits
        ]

roundUnits :: TestTree
roundUnits = testGroup "Rounding ..."
    [ testGroup "Rounding to 2 decimal places"
        [ HU.testCase "123456789.0 => no change" $
            dpRound 2 (toRational (123456789.0 :: Double)) @?= 123456789.0
        , HU.testCase "1234.56789 => 1234.57" $
            dpRound 2 (toRational (1234.56789 :: Double)) @?= 1234.57
        , HU.testCase "123.456789 => 123.46" $
            dpRound 2 (toRational (123.456789 :: Double)) @?= 123.46
        , HU.testCase "12.3456789 => 12.35" $
            dpRound 2 (toRational (12.3456789 :: Double)) @?= 12.35
        , HU.testCase "1.23456789 => 1.23" $
            dpRound 2 (toRational (1.23456789 :: Double)) @?= 1.23
        , HU.testCase "0.123456789 => 0.12" $
            dpRound 2 (toRational (0.123456789 :: Double)) @?= 0.12
        , HU.testCase "0.0123456789 => 0.01" $
            dpRound 2 (toRational (0.0123456789 :: Double)) @?= 0.01
        , HU.testCase "0.0000123456789 => 0.0" $
            dpRound 2 (toRational (0.0000123456789 :: Double)) @?= 0.0
        ]
    , testGroup "Rounding 4 significant digits"
        [ HU.testCase "123456789.0 => 123500000.0" $
            sdRound 4 (toRational (123456789.0 :: Double)) @?= 123500000.0
        , HU.testCase "1234.56789 => 1235.0" $
            sdRound 4 (toRational (1234.56789 :: Double)) @?= 1235.0
        , HU.testCase "123.456789 => 123.5" $
            sdRound 4 (toRational (123.456789 :: Double)) @?= 123.5
        , HU.testCase "12.3456789 => 12.35" $
            sdRound 4 (toRational (12.3456789 :: Double)) @?= 12.35
        , HU.testCase "1.23456789 => 1.235" $
            sdRound 4 (toRational (1.23456789 :: Double)) @?= 1.235
        , HU.testCase "0.123456789 => 0.1235" $
            sdRound 4 (toRational (0.123456789 :: Double)) @?= 0.1235
        , HU.testCase "0.0123456789 => 0.01235" $
            sdRound 4 (toRational (0.0123456789 :: Double)) @?= 0.01235
        , HU.testCase "0.0000123456789 => 0.00001235" $
            sdRound 4 (toRational (0.0000123456789 :: Double)) @?= 0.00001235
        ]
    ]

