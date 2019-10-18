module Main (main) where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit as HU ((@?=), testCase)
import System.FilePath (takeExtensions)

import Flight.Path

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ extUnits
        , ensureExtUnits
        ]

extUnits :: TestTree
extUnits = testGroup "Set of file extensions"
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

ensureExtUnits :: TestTree
ensureExtUnits = testGroup "Ensure file extensions"
    [ testGroup "As fsdb"
        [ HU.testCase "x.FSDB"
        $ takeExtensions (ensureExt Fsdb "x.FSDB") @?= ".fsdb"

        , HU.testCase "x.fsdb"
        $ takeExtensions (ensureExt Fsdb "x.fsdb") @?= ".fsdb"

        , HU.testCase "x"
        $ takeExtensions (ensureExt Fsdb "x") @?= ".fsdb"

        , HU.testCase "x.NOT"
        $ takeExtensions (ensureExt Fsdb "x.NOT") @?= ".fsdb"
        ]
    , testGroup "As kml"
        [ HU.testCase "x.KML"
        $ takeExtensions (ensureExt Kml "x.KML") @?= ".kml"

        , HU.testCase "x.kml"
        $ takeExtensions (ensureExt Kml "x.kml") @?= ".kml"

        , HU.testCase "x"
        $ takeExtensions (ensureExt Kml "x") @?= ".kml"

        , HU.testCase "x.NOT"
        $ takeExtensions (ensureExt Kml "x.NOT") @?= ".kml"
        ]
    , testGroup "As igc"
        [ HU.testCase "x.IGC"
        $ takeExtensions (ensureExt Igc "x.IGC") @?= ".igc"

        , HU.testCase "x.igc"
        $ takeExtensions (ensureExt Igc "x.igc") @?= ".igc"

        , HU.testCase "x"
        $ takeExtensions (ensureExt Igc "x") @?= ".igc"

        , HU.testCase "x.NOT"
        $ takeExtensions (ensureExt Igc "x.NOT") @?= ".igc"
        ]
    , testGroup "As comp-input.yaml"
        [ HU.testCase "x.comp-input.yaml"
        $ takeExtensions (ensureExt CompInput "x.comp-input.yaml") @?= ".comp-input.yaml"

        , HU.testCase "x.comp-input"
        $ takeExtensions (ensureExt CompInput "x.comp-input") @?= ".comp-input.yaml"

        , HU.testCase "x"
        $ takeExtensions (ensureExt CompInput "x") @?= ".comp-input.yaml"

        , HU.testCase "x.NOT"
        $ takeExtensions (ensureExt CompInput "x.NOT") @?= ".comp-input.yaml"
        ]
    , testGroup "As task-length.yaml"
        [ HU.testCase "x.task-length.yaml"
        $ takeExtensions (ensureExt TaskLength "x.task-length.yaml") @?= ".task-length.yaml"

        , HU.testCase "x.task-length"
        $ takeExtensions (ensureExt TaskLength "x.task-length") @?= ".task-length.yaml"

        , HU.testCase "x"
        $ takeExtensions (ensureExt TaskLength "x") @?= ".task-length.yaml"

        , HU.testCase "x.NOT"
        $ takeExtensions (ensureExt TaskLength "x.NOT") @?= ".task-length.yaml"
        ]
    , testGroup "As cross-zone.yaml"
        [ HU.testCase "x.cross-zone.yaml"
        $ takeExtensions (ensureExt CrossZone "x.cross-zone.yaml") @?= ".cross-zone.yaml"

        , HU.testCase "x.cross-zone"
        $ takeExtensions (ensureExt CrossZone "x.cross-zone") @?= ".cross-zone.yaml"

        , HU.testCase "x"
        $ takeExtensions (ensureExt CrossZone "x") @?= ".cross-zone.yaml"

        , HU.testCase "x.NOT"
        $ takeExtensions (ensureExt CrossZone "x.NOT") @?= ".cross-zone.yaml"
        ]
    , testGroup "As tag-zone.yaml"
        [ HU.testCase "x.tag-zone.yaml"
        $ takeExtensions (ensureExt TagZone "x.tag-zone.yaml") @?= ".tag-zone.yaml"

        , HU.testCase "x.tag-zone"
        $ takeExtensions (ensureExt TagZone "x.tag-zone") @?= ".tag-zone.yaml"

        , HU.testCase "x"
        $ takeExtensions (ensureExt TagZone "x") @?= ".tag-zone.yaml"

        , HU.testCase "x.NOT"
        $ takeExtensions (ensureExt TagZone "x.NOT") @?= ".tag-zone.yaml"
        ]
    , testGroup "As align-time.yaml"
        [ HU.testCase "x.align-time.yaml"
        $ takeExtensions (ensureExt AlignTime "x.align-time.yaml") @?= ".align-time.yaml"

        , HU.testCase "x.align-time"
        $ takeExtensions (ensureExt AlignTime "x.align-time") @?= ".align-time.yaml"

        , HU.testCase "x"
        $ takeExtensions (ensureExt AlignTime "x") @?= ".align-time.yaml"

        , HU.testCase "x.NOT"
        $ takeExtensions (ensureExt AlignTime "x.NOT") @?= ".align-time.yaml"
        ]
    , testGroup "As discard-further.yaml"
        [ HU.testCase "x.discard-further.yaml"
        $ takeExtensions (ensureExt DiscardFurther "x.discard-further.yaml") @?= ".discard-further.yaml"

        , HU.testCase "x.discard-further"
        $ takeExtensions (ensureExt DiscardFurther "x.discard-further") @?= ".discard-further.yaml"

        , HU.testCase "x"
        $ takeExtensions (ensureExt DiscardFurther "x") @?= ".discard-further.yaml"

        , HU.testCase "x.NOT"
        $ takeExtensions (ensureExt DiscardFurther "x.NOT") @?= ".discard-further.yaml"
        ]
    , testGroup "As mask-track.yaml"
        [ HU.testCase "x.mask-track.yaml"
        $ takeExtensions (ensureExt MaskTrack "x.mask-track.yaml") @?= ".mask-track.yaml"

        , HU.testCase "x.mask-track"
        $ takeExtensions (ensureExt MaskTrack "x.mask-track") @?= ".mask-track.yaml"

        , HU.testCase "x"
        $ takeExtensions (ensureExt MaskTrack "x") @?= ".mask-track.yaml"

        , HU.testCase "x.NOT"
        $ takeExtensions (ensureExt MaskTrack "x.NOT") @?= ".mask-track.yaml"
        ]
    , testGroup "As land-out.yaml"
        [ HU.testCase "x.land-out.yaml"
        $ takeExtensions (ensureExt LandOut "x.land-out.yaml") @?= ".land-out.yaml"

        , HU.testCase "x.land-out"
        $ takeExtensions (ensureExt LandOut "x.land-out") @?= ".land-out.yaml"

        , HU.testCase "x"
        $ takeExtensions (ensureExt LandOut "x") @?= ".land-out.yaml"

        , HU.testCase "x.NOT"
        $ takeExtensions (ensureExt LandOut "x.NOT") @?= ".land-out.yaml"
        ]
    , testGroup "As gap-point.yaml"
        [ HU.testCase "x.gap-point.yaml"
        $ takeExtensions (ensureExt GapPoint "x.gap-paint.yaml") @?= ".gap-point.yaml"

        , HU.testCase "x.gap-point"
        $ takeExtensions (ensureExt GapPoint "x.gap-point") @?= ".gap-point.yaml"

        , HU.testCase "x"
        $ takeExtensions (ensureExt GapPoint "x") @?= ".gap-point.yaml"

        , HU.testCase "x.NOT"
        $ takeExtensions (ensureExt GapPoint "x.NOT") @?= ".gap-point.yaml"
        ]
    ]
