{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module TimeValidity
    ( timeValidityUnits
    , scTimeValidity
    , qcTimeValidity
    ) where

import qualified Flight.Score as FS
import Flight.Score
    ( TimeValidity(..)
    , NominalTime(..)
    , NominalDistance(..)
    , Seconds
    , Metres
    , isNormal
    )

import Test.Tasty (TestTree, testGroup)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

timeValidityUnits :: TestTree
timeValidityUnits = testGroup "Time validity unit tests"
    [ HU.testCase "Time validity 0 0 (Just 0) 0 = 0" $
        FS.timeValidity
            (NominalTime 0)
            (NominalDistance . MkQuantity $ 0)
            (Just 0)
            0
        @?= TimeValidity 0

    , HU.testCase "Time validity 1 0 (Just 1) 0 = 1" $
        FS.timeValidity
            (NominalTime 1)
            (NominalDistance . MkQuantity $ 0)
            (Just 1)
            0
        @?= TimeValidity 1

    , HU.testCase "Time validity 1 1 (Just 1) 1 = 1" $
        FS.timeValidity
            (NominalTime 1)
            (NominalDistance . MkQuantity $ 1)
            (Just 1)
            1
        @?= TimeValidity 1

    , HU.testCase "Time validity 0 0 Nothing 0 = 0" $
        FS.timeValidity
            (NominalTime 0)
            (NominalDistance . MkQuantity $ 0)
            Nothing
            0
        @?= TimeValidity 0

    , HU.testCase "Time validity 0 1 Nothing 1 = 1" $
        FS.timeValidity
            (NominalTime 0)
            (NominalDistance . MkQuantity $ 1)
            Nothing
            1
        @?= TimeValidity 1

    , HU.testCase "Time validity 1 1 Nothing 1 = 1" $
        FS.timeValidity
            (NominalTime 1)
            (NominalDistance . MkQuantity $ 1)
            Nothing
            1
        @?= TimeValidity 1
    ]

timeValidity
    :: NominalTime
    -> NominalDistance (Quantity Double [u| km |])
    -> Maybe Seconds
    -> Metres
    -> Bool
timeValidity nt nd t d =
    (\(TimeValidity x) -> isNormal x) $ FS.timeValidity nt nd t d

scTimeValidity
    :: SC.NonNegative Integer
    -> SC.NonNegative Double
    -> Maybe (SC.NonNegative Seconds)
    -> SC.NonNegative Metres
    -> Bool
scTimeValidity
    (SC.NonNegative nt) (SC.NonNegative nd) Nothing (SC.NonNegative d) =
    timeValidity
        (NominalTime nt)
        (NominalDistance . MkQuantity $ nd)
        Nothing
        d

scTimeValidity
    (SC.NonNegative nt)
    (SC.NonNegative nd)
    (Just (SC.NonNegative t))
    (SC.NonNegative d) =
    timeValidity
        (NominalTime nt)
        (NominalDistance . MkQuantity $ nd)
        (Just t)
        d

qcTimeValidity
    :: QC.NonNegative Integer
    -> QC.NonNegative Double
    -> Maybe (QC.NonNegative Seconds)
    -> QC.NonNegative Metres
    -> Bool
qcTimeValidity
    (QC.NonNegative nt) (QC.NonNegative nd) Nothing (QC.NonNegative d) =
    timeValidity
        (NominalTime nt)
        (NominalDistance . MkQuantity $ nd)
        Nothing
        d

qcTimeValidity
    (QC.NonNegative nt)
    (QC.NonNegative nd)
    (Just (QC.NonNegative t))
    (QC.NonNegative d) =
    timeValidity
        (NominalTime nt)
        (NominalDistance . MkQuantity $ nd)
        (Just t)
        d
