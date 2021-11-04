module StopValidity (stopValidityUnits, stopValidity) where

import Data.Function ((&))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import Data.UnitsOfMeasure (u)

import "flight-gap-allot" Flight.Score
    ( PilotsAtEss(..)
    , PilotsLanded(..)
    , PilotsFlying(..)
    , LaunchToEss(..)
    , isNormal
    )
import qualified "flight-gap-valid" Flight.Score as FS
import "flight-gap-valid" Flight.Score (StopValidity(..))

import TestNewtypes

stopValidityUnits :: TestTree
stopValidityUnits = testGroup "Is a stopped task valid?"
    [ HU.testCase "Not when no one launches = 0 validity" $
        (FS.stopValidity
            (PilotsFlying 0)
            (PilotsAtEss 0)
            (PilotsLanded 0)
            (PilotsFlying 1)
            (mkReachStats [])
            (LaunchToEss [u| 1 km |]) & fst)
            @?= StopValidity 0

    , HU.testCase "When everyone makes ESS, one pilot launched and is still flying = 1 validity" $
        (FS.stopValidity
            (PilotsFlying 1)
            (PilotsAtEss 1)
            (PilotsLanded 0)
            (PilotsFlying 1)
            (mkReachStats [1])
            (LaunchToEss [u| 1 km |]) & fst)
            @?= StopValidity 1

    , HU.testCase "When everyone makes ESS, one pilot launched and has landed = 1 validity" $
        (FS.stopValidity
            (PilotsFlying 1)
            (PilotsAtEss 2)
            (PilotsLanded 1)
            (PilotsFlying 0)
            (mkReachStats [1])
            (LaunchToEss [u| 1 km |]) & fst)
            @?= StopValidity 1

    , HU.testCase "When everyone makes ESS, two pilots launched, both still flying = 1 validity" $
        (FS.stopValidity
            (PilotsFlying 2)
            (PilotsAtEss 2)
            (PilotsLanded 0)
            (PilotsFlying 2)
            (mkReachStats [1, 1])
            (LaunchToEss [u| 1 km |]) & fst)
            @?= StopValidity 1

    , HU.testCase "When everyone makes ESS, two pilots launched, noone still flying = 1 validity" $
        (FS.stopValidity
            (PilotsFlying 2)
            (PilotsAtEss 2)
            (PilotsLanded 2)
            (PilotsFlying 0)
            (mkReachStats [1, 1])
            (LaunchToEss [u| 1 km |]) & fst)
            @?= StopValidity 1

    , HU.testCase "When everyone makes ESS, two pilots launched, one still flying = 1 validity" $
        (FS.stopValidity
            (PilotsFlying 2)
            (PilotsAtEss 2)
            (PilotsLanded 1)
            (PilotsFlying 1)
            (mkReachStats [1, 1])
            (LaunchToEss [u| 1 km |]) & fst)
            @?= StopValidity 1

    , HU.testCase "When one makes ESS, one still flying at launch point = 1 validity" $
        (FS.stopValidity
            (PilotsFlying 2)
            (PilotsAtEss 1)
            (PilotsLanded 0)
            (PilotsFlying 1)
            (mkReachStats [1, 0])
            (LaunchToEss [u| 1 km |]) & fst)
            @?= StopValidity 1

    , HU.testCase "When one makes ESS, one still flying on course halfway to ESS = 1 validity" $
        (FS.stopValidity
            (PilotsFlying 2)
            (PilotsAtEss 1)
            (PilotsLanded 0)
            (PilotsFlying 1)
            (mkReachStats [2, 1])
            (LaunchToEss [u| 2 km |]) & fst)
            @?= StopValidity 1
    ]

stopValidity :: StopValidityTest -> Bool
stopValidity (StopValidityTest ((launched, atEss, landed, stillFlying, distance), reach)) =
    (\(StopValidity x, _) -> isNormal x) $ FS.stopValidity launched atEss landed stillFlying reach distance
