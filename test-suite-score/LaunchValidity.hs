module LaunchValidity
    ( launchValidityUnits
    , scLaunchValidity
    , qcLaunchValidity
    ) where

import qualified Flight.Score as FS
import Flight.Score
    ( NominalLaunch(..)
    , LaunchValidity(..)
    , isNormal
    )

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

launchValidityUnits :: TestTree
launchValidityUnits = testGroup "Launch validity unit tests"
    [ HU.testCase "Launch validity 0 0 = 0" $
        FS.launchValidity (NominalLaunch (0 % 1)) (0 % 1) @?= LaunchValidity (0 % 1)

    , HU.testCase "Launch validity 1 0 = 0" $
        FS.launchValidity (NominalLaunch (1 % 1)) (0 % 1) @?= LaunchValidity (0 % 1)

    , HU.testCase "Launch validity 0 1 = 1" $
        FS.launchValidity (NominalLaunch (0 % 1)) (1 % 1) @?= LaunchValidity (1 % 1)

    , HU.testCase "Launch validity 1 1 = 1" $
        FS.launchValidity (NominalLaunch (1 % 1)) (1 % 1) @?= LaunchValidity (1 % 1)
    ]

launchValidity :: Integer -> Integer -> Integer -> Integer -> Bool
launchValidity nx dx ny dy =
    let nominalLaunch = NominalLaunch (nx % dx)
        fractionLaunching = ny % dy
    in (\(LaunchValidity x) -> isNormal x) $ FS.launchValidity nominalLaunch fractionLaunching

scLaunchValidity :: Monad m =>
                    SC.NonNegative Integer
                    -> SC.Positive Integer
                    -> SC.NonNegative Integer
                    -> SC.Positive Integer
                    -> SC.Property m
scLaunchValidity
    (SC.NonNegative nx)
    (SC.Positive dx)
    (SC.NonNegative ny)
    (SC.Positive dy) =
    nx <= dx && ny <= dy SC.==> launchValidity nx dx ny dy

qcLaunchValidity :: QC.NonNegative Integer
                    -> QC.Positive Integer
                    -> QC.NonNegative Integer
                    -> QC.Positive Integer
                    -> QC.Property
qcLaunchValidity
    (QC.NonNegative nx)
    (QC.Positive dx)
    (QC.NonNegative ny)
    (QC.Positive dy) =
    nx <= dx && ny <= dy QC.==> launchValidity nx dx ny dy
