{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DistanceValidity
    ( distanceValidityUnits
    , scDistanceValidity
    , qcDistanceValidity
    ) where

import qualified Flight.Score as FS
import Flight.Score
    ( NominalGoal(..)
    , NominalDistance(..)
    , Metres
    , DistanceValidity(..)
    , isNormal
    )

import Test.Tasty (TestTree, testGroup)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

import Normal (Normal(..))

distanceValidityUnits :: TestTree
distanceValidityUnits = testGroup "Distance validity unit tests"
    [ HU.testCase "Distance validity 0 0 0 0 0 0 == 0" $
        FS.distanceValidity (NominalGoal (0 % 1)) (NominalDistance 0) 0 0 0 0
        @?= DistanceValidity (0 % 1)

    , HU.testCase "Distance validity 1 1 1 1 1 1 == 1" $
        FS.distanceValidity (NominalGoal (1 % 1)) (NominalDistance 1) 1 1 1 1
        @?= DistanceValidity (1 % 1)

    , HU.testCase "Distance validity 1 0 1 1 1 1 == 1" $
        FS.distanceValidity (NominalGoal (1 % 1)) (NominalDistance 0) 1 1 1 1
        @?= DistanceValidity (1 % 1)

    , HU.testCase "Distance validity 1 1 0 1 1 1 == 0" $
        FS.distanceValidity (NominalGoal (1 % 1)) (NominalDistance 1) 0 1 1 1
        @?= DistanceValidity (0 % 1)

    , HU.testCase "Distance validity 1 1 1 0 1 1 == 1" $
        FS.distanceValidity (NominalGoal (1 % 1)) (NominalDistance 1) 1 0 1 1
        @?= DistanceValidity (1 % 1)

    , HU.testCase "Distance validity 1 1 1 1 0 1 == 0" $
        FS.distanceValidity (NominalGoal (1 % 1)) (NominalDistance 1) 1 1 0 1
        @?= DistanceValidity (0 % 1)

    , HU.testCase "Distance validity 1 1 1 1 1 0 == 0" $
        FS.distanceValidity (NominalGoal (1 % 1)) (NominalDistance 1) 1 1 1 0
        @?= DistanceValidity (0 % 1)
    ]

newtype NgTest = NgTest NominalGoal deriving Show
newtype NdTest = NdTest NominalDistance deriving Show

distanceValidity :: NgTest
                    -> NdTest -> Integer -> Metres -> Metres -> Metres -> Bool
distanceValidity (NgTest ng) (NdTest nd) nFly dMin dMax dSum =
    (\(DistanceValidity x) -> isNormal x) $ FS.distanceValidity ng nd nFly dMin dMax dSum

scDistanceValidity :: NgTest
                      -> NdTest
                      -> SC.NonNegative Integer
                      -> SC.NonNegative Metres
                      -> SC.NonNegative Metres
                      -> SC.NonNegative Metres
                      -> Bool
scDistanceValidity
    ng
    nd
    (SC.NonNegative nFly)
    (SC.NonNegative dMin)
    (SC.NonNegative dMax)
    (SC.NonNegative dSum) =
    distanceValidity ng nd nFly dMin dMax dSum

qcDistanceValidity :: NgTest
                      -> NdTest
                      -> QC.NonNegative Integer
                      -> QC.NonNegative Metres
                      -> QC.NonNegative Metres
                      -> QC.NonNegative Metres
                      -> Bool
qcDistanceValidity
    ng
    nd
    (QC.NonNegative nFly)
    (QC.NonNegative dMin)
    (QC.NonNegative dMax)
    (QC.NonNegative dSum) =
    distanceValidity ng nd nFly dMin dMax dSum

instance Monad m => SC.Serial m NgTest where
    series = cons1 $ \(Normal x) -> NgTest (NominalGoal x)

instance QC.Arbitrary NgTest where
    arbitrary = arbitrary >>= \(Normal x) -> return $ NgTest (NominalGoal x)

instance Monad m => SC.Serial m NdTest where
    series = cons1 $ \(SC.NonNegative x) -> NdTest (NominalDistance x)

instance QC.Arbitrary NdTest where
    arbitrary = arbitrary >>= \(QC.NonNegative x) -> return $ NdTest (NominalDistance x)
