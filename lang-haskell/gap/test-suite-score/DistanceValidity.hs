module DistanceValidity
    ( distanceValidityUnits
    , scDistanceValidity
    , qcDistanceValidity
    ) where

import Test.Tasty (TestTree, testGroup)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import qualified Flight.Score as FS
import Flight.Score
    ( NominalGoal(..)
    , NominalDistance(..)
    , DistanceValidity(..)
    , MinimumDistance(..)
    , MaximumDistance(..)
    , SumOfDistance(..)
    , PilotsFlying(..)
    , isNormal
    )

import TestNewtypes

distanceValidityUnits :: TestTree
distanceValidityUnits = testGroup "Distance validity unit tests"
    [ HU.testCase
        "Distance validity nom-goal=0 nom-dist=0 flying=0 min=0 max=0 sum=0 => 0" $
        fst
        (FS.distanceValidity
            (NominalGoal 0)
            (NominalDistance . MkQuantity $ 0)
            (PilotsFlying 0)
            (MinimumDistance . MkQuantity $ 0)
            (MaximumDistance . MkQuantity $ 0)
            (SumOfDistance . MkQuantity $ 0))
        @?= DistanceValidity (0 % 1)

    , HU.testCase
        "Distance validity nom-goal=1 nom-dist=1 flying=1 min=1 max=1 sum=1 => 0" $
        fst
        (FS.distanceValidity
            (NominalGoal 1)
            (NominalDistance . MkQuantity $ 1)
            (PilotsFlying 1)
            (MinimumDistance . MkQuantity $ 1)
            (MaximumDistance . MkQuantity $ 1)
            (SumOfDistance . MkQuantity $ 1))
        @?= DistanceValidity (0 % 1)

    , HU.testCase
    "Distance validity nom-goal=1 nom-dist=0 flying=1 min=1 max=1 sum=1 => 1" $

        fst
        (FS.distanceValidity
            (NominalGoal 1)
            (NominalDistance . MkQuantity $ 0)
            (PilotsFlying 1)
            (MinimumDistance . MkQuantity $ 1)
            (MaximumDistance . MkQuantity $ 1)
            (SumOfDistance . MkQuantity $ 1))
        @?= DistanceValidity (1 % 1)

    , HU.testCase
        "Distance validity nom-goal=1 nom-dist=1 flying=0 min=1 max=1 sum=1 => 0" $
        fst
        (FS.distanceValidity
            (NominalGoal 1)
            (NominalDistance . MkQuantity $ 1)
            (PilotsFlying 0)
            (MinimumDistance . MkQuantity $ 1)
            (MaximumDistance . MkQuantity $ 1)
            (SumOfDistance . MkQuantity $ 1))
        @?= DistanceValidity (0 % 1)

    , HU.testCase
        "Distance validity nom-goal=1 nom-dist=1 flying=1 min=0 max=1 sum=1 => 1" $
        fst
        (FS.distanceValidity
            (NominalGoal 1)
            (NominalDistance . MkQuantity $ 1)
            (PilotsFlying 1)
            (MinimumDistance . MkQuantity $ 0)
            (MaximumDistance . MkQuantity $ 1)
            (SumOfDistance . MkQuantity $ 1))
        @?= DistanceValidity (1 % 1)

    , HU.testCase
        "Distance validity nom-goal=1 nom-dist=1 flying=1 min=1 max=0 sum=1 => 0" $
        fst
        (FS.distanceValidity
            (NominalGoal 1)
            (NominalDistance . MkQuantity $ 1)
            (PilotsFlying 1)
            (MinimumDistance . MkQuantity $ 1)
            (MaximumDistance . MkQuantity $ 0)
            (SumOfDistance . MkQuantity $ 1))
        @?= DistanceValidity (0 % 1)

    , HU.testCase
        "Distance validity nom-goal=1 nom-dist=1 flying=1 min=1 max=1 sum=0 => 0" $
        fst
        (FS.distanceValidity
            (NominalGoal 1)
            (NominalDistance . MkQuantity $ 1)
            (PilotsFlying 1)
            (MinimumDistance . MkQuantity $ 1)
            (MaximumDistance . MkQuantity $ 1)
            (SumOfDistance . MkQuantity $ 0))
        @?= DistanceValidity (0 % 1)
    ]

distanceValidity
    :: NgTest
    -> NdTest
    -> PilotsFlying
    -> MinimumDistance (Quantity Double [u| km |])
    -> MaximumDistance (Quantity Double [u| km |])
    -> SumOfDistance (Quantity Double [u| km |])
    -> Bool
distanceValidity
    (NgTest ng)
    (NdTest nd)
    nFly
    dMin
    dMax
    dSum =
    (\(DistanceValidity x) -> isNormal x) . fst
    $ FS.distanceValidity ng nd nFly dMin dMax dSum

scDistanceValidity :: NgTest
                      -> NdTest
                      -> SC.NonNegative Integer
                      -> SC.NonNegative Double
                      -> SC.NonNegative Double
                      -> SC.NonNegative Double
                      -> Bool
scDistanceValidity
    ng
    nd
    (SC.NonNegative nFly)
    (SC.NonNegative dMin)
    (SC.NonNegative dMax)
    (SC.NonNegative dSum) =
    distanceValidity
        ng
        nd
        (PilotsFlying nFly)
        (MinimumDistance . MkQuantity $ dMin)
        (MaximumDistance . MkQuantity $ dMax)
        (SumOfDistance . MkQuantity $ dSum)

qcDistanceValidity :: NgTest
                      -> NdTest
                      -> QC.NonNegative Integer
                      -> QC.NonNegative Double
                      -> QC.NonNegative Double
                      -> QC.NonNegative Double
                      -> Bool
qcDistanceValidity
    ng
    nd
    (QC.NonNegative nFly)
    (QC.NonNegative dMin)
    (QC.NonNegative dMax)
    (QC.NonNegative dSum) =
    distanceValidity
        ng
        nd
        (PilotsFlying nFly)
        (MinimumDistance . MkQuantity $ dMin)
        (MaximumDistance . MkQuantity $ dMax)
        (SumOfDistance . MkQuantity $ dSum)
