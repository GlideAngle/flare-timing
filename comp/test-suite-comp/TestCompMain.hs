module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit as HU ((@?=), testCase)

import Flight.Path

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ extUnits
        ]

extUnits :: TestTree
extUnits = testGroup "File extensions"
    [ HU.testCase ".fsdb" $ ext Fsdb @?= ".fsdb"
    , HU.testCase ".kml" $ ext Kml @?= ".kml"
    , HU.testCase ".igc" $ ext Igc @?= ".igc"
    , HU.testCase ".comp-input.yaml" $ ext CompInput @?= ".comp-input.yaml"
    , HU.testCase ".task-length.yaml" $ ext TaskLength @?= ".task-length.yaml"
    , HU.testCase ".cross-zone.yaml" $ ext CrossZone @?= ".cross-zone.yaml"
    , HU.testCase ".tag-zone.yaml" $ ext TagZone @?= ".tag-zone.yaml"
    , HU.testCase ".align-time.yaml" $ ext AlignTime @?= ".align-time.yaml"
    , HU.testCase ".discard-further.yaml" $ ext DiscardFurther @?= ".discard-further.yaml"
    , HU.testCase ".mask-track.yaml" $ ext MaskTrack @?= ".mask-track.yaml"
    , HU.testCase ".land-out.yaml" $ ext LandOut @?= ".land-out.yaml"
    , HU.testCase ".gap-point.yaml" $ ext GapPoint @?= ".gap-point.yaml"
    ]
