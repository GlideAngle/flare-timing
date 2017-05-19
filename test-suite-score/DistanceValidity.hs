module DistanceValidity
    ( distanceValidityUnits
    , scDistanceValidity
    , qcDistanceValidity
    ) where

import qualified Flight.Score as FS
import Flight.Score (NominalDistance, Metres, isNormal)

import Test.Tasty (TestTree, testGroup)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

distanceValidityUnits :: TestTree
distanceValidityUnits = testGroup "distance validity unit tests"
    [ HU.testCase "distance validity 0 0 0 0 0 0 == 0" $
        FS.distanceValidity (0 % 1) 0 0 0 0 0 @?= (0 % 1)

    , HU.testCase "distance validity 1 1 1 1 1 1 == 1" $
        FS.distanceValidity (1 % 1) 1 1 1 1 1 @?= (1 % 1)

    , HU.testCase "distance validity 1 0 1 1 1 1 == 1" $
        FS.distanceValidity (1 % 1) 0 1 1 1 1 @?= (1 % 1)

    , HU.testCase "distance validity 1 1 0 1 1 1 == 0" $
        FS.distanceValidity (1 % 1) 1 0 1 1 1 @?= (0 % 1)

    , HU.testCase "distance validity 1 1 1 0 1 1 == 1" $
        FS.distanceValidity (1 % 1) 1 1 0 1 1 @?= (1 % 1)

    , HU.testCase "distance validity 1 1 1 1 0 1 == 0" $
        FS.distanceValidity (1 % 1) 1 1 1 0 1 @?= (0 % 1)

    , HU.testCase "distance validity 1 1 1 1 1 0 == 0" $
        FS.distanceValidity (1 % 1) 1 1 1 1 0 @?= (0 % 1)
    ]

distanceValidity :: FS.NominalGoal
                    -> NominalDistance -> Integer -> Metres -> Metres -> Metres -> Bool
distanceValidity ng nd nFly dMin dMax dSum =
    isNormal $ FS.distanceValidity ng nd nFly dMin dMax dSum

scDistanceValidity :: SC.NonNegative FS.NominalGoal
                      -> SC.NonNegative NominalDistance
                      -> SC.NonNegative Integer
                      -> SC.NonNegative Metres
                      -> SC.NonNegative Metres
                      -> SC.NonNegative Metres
                      -> Bool
scDistanceValidity
    (SC.NonNegative ng)
    (SC.NonNegative nd)
    (SC.NonNegative nFly)
    (SC.NonNegative dMin)
    (SC.NonNegative dMax)
    (SC.NonNegative dSum) =
    distanceValidity ng nd nFly dMin dMax dSum

qcDistanceValidity :: QC.NonNegative FS.NominalGoal
                      -> QC.NonNegative NominalDistance
                      -> QC.NonNegative Integer
                      -> QC.NonNegative Metres
                      -> QC.NonNegative Metres
                      -> QC.NonNegative Metres
                      -> Bool
qcDistanceValidity
    (QC.NonNegative ng)
    (QC.NonNegative nd)
    (QC.NonNegative nFly)
    (QC.NonNegative dMin)
    (QC.NonNegative dMax)
    (QC.NonNegative dSum) =
    distanceValidity ng nd nFly dMin dMax dSum
