module LaunchValidity
    ( launchValidityUnits
    , scLaunchValidity
    , qcLaunchValidity
    ) where

import "flight-gap-base" Flight.Score
    ( NominalLaunch(..)
    , PilotsPresent(..)
    , PilotsFlying(..)
    , isNormal
    )
import qualified "flight-gap-valid" Flight.Score as FS
import "flight-gap-valid" Flight.Score (LaunchValidity(..))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.Ratio ((%))

launchValidityUnits :: TestTree
launchValidityUnits = testGroup "Launch validity unit tests"
    [ HU.testCase "Launch validity nom-launch=0 present=1 flying=0 = 0" $
        fst
        (FS.launchValidity
            (NominalLaunch 0)
            (PilotsPresent 1)
            (PilotsFlying 0))
        @?= LaunchValidity 0

    , HU.testCase "Launch validity nom-launch=1 present=1 flying=0 = 0" $
        fst
        (FS.launchValidity
            (NominalLaunch 1)
            (PilotsPresent 1)
            (PilotsFlying 0))
        @?= LaunchValidity 0

    , HU.testCase "Launch validity nom-launch=0 present=1 flying=1 = 1" $
        fst
        (FS.launchValidity
            (NominalLaunch 0)
            (PilotsPresent 1)
            (PilotsFlying 1))
        @?= LaunchValidity 1

    , HU.testCase "Launch validity nom-launch=1 present=1 flying=1 = 1" $
        fst
        (FS.launchValidity
            (NominalLaunch 1)
            (PilotsPresent 1)
            (PilotsFlying 1))
        @?= LaunchValidity 1
    ]

launchValidity :: Integer -> Integer -> PilotsPresent -> PilotsFlying -> Bool
launchValidity nx dx np nf =
    (\(LaunchValidity x) -> isNormal x) . fst
    $ FS.launchValidity nominalLaunch np nf
    where
        nominalLaunch = NominalLaunch (nx % dx)

scLaunchValidity
    :: Monad m
    => SC.NonNegative Integer
    -> SC.Positive Integer
    -> SC.NonNegative Integer
    -> SC.Positive Integer
    -> SC.Property m
scLaunchValidity
    (SC.NonNegative nx)
    (SC.Positive dx)
    (SC.NonNegative nf)
    (SC.Positive np) =
    nx <= dx && nf <= np SC.==>
        launchValidity nx dx (PilotsPresent np) (PilotsFlying nf)

qcLaunchValidity
    :: QC.NonNegative Integer
    -> QC.Positive Integer
    -> QC.NonNegative Integer
    -> QC.Positive Integer
    -> QC.Property
qcLaunchValidity
    (QC.NonNegative nx)
    (QC.Positive dx)
    (QC.NonNegative nf)
    (QC.Positive np) =
    nx <= dx && nf <= np QC.==>
        launchValidity nx dx (PilotsPresent np) (PilotsFlying nf)
