module TimeValidity
    ( timeValidityUnits
    , scTimeValidity
    , qcTimeValidity
    ) where

import qualified Flight.Score as FS
import Flight.Score (NominalTime, NominalDistance, Seconds, Metres, isNormal)

import Test.Tasty (TestTree, testGroup)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

timeValidityUnits :: TestTree
timeValidityUnits = testGroup "Time validity unit tests"
    [ HU.testCase "Time validity 0 0 (Just 0) 0 == 0" $
        FS.timeValidity 0 0 (Just 0) 0 @?= (0 % 1)

    , HU.testCase "Time validity 1 0 (Just 1) 0 == 1" $
        FS.timeValidity 1 0 (Just 1) 0 @?= (1 % 1)

    , HU.testCase "Time validity 1 1 (Just 1) 1 == 1" $
        FS.timeValidity 1 1 (Just 1) 1 @?= (1 % 1)

    , HU.testCase "Time validity 0 0 Nothing 0 == 0" $
        FS.timeValidity 0 0 Nothing 0 @?= (0 % 1)

    , HU.testCase "Time validity 0 1 Nothing 1 == 1" $
        FS.timeValidity 0 1 Nothing 1 @?= (1 % 1)

    , HU.testCase "Time validity 1 1 Nothing 1 == 1" $
        FS.timeValidity 1 1 Nothing 1 @?= (1 % 1)
    ]

timeValidity :: NominalTime -> NominalDistance -> Maybe Seconds -> Metres -> Bool
timeValidity nt nd t d = isNormal $ FS.timeValidity nt nd t d

scTimeValidity :: SC.NonNegative NominalTime
                  -> SC.NonNegative NominalDistance
                  -> Maybe (SC.NonNegative Seconds)
                  -> SC.NonNegative Metres
                  -> Bool
scTimeValidity (SC.NonNegative nt) (SC.NonNegative nd) Nothing (SC.NonNegative d) =
    timeValidity nt nd Nothing d
scTimeValidity (SC.NonNegative nd) (SC.NonNegative nt) (Just (SC.NonNegative t)) (SC.NonNegative d) =
    timeValidity nt nd (Just t) d

qcTimeValidity :: QC.NonNegative NominalTime
                  -> QC.NonNegative NominalDistance
                  -> Maybe (QC.NonNegative Seconds)
                  -> QC.NonNegative Metres
                  -> Bool
qcTimeValidity (QC.NonNegative nt) (QC.NonNegative nd) Nothing (QC.NonNegative d) =
    timeValidity nt nd Nothing d
qcTimeValidity (QC.NonNegative nd) (QC.NonNegative nt) (Just (QC.NonNegative t)) (QC.NonNegative d) =
    timeValidity nt nd (Just t) d
