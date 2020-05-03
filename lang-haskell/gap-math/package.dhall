    let defs = ./../defaults.dhall

in  let testdeps =
          [ "base"
          , "containers"
          , "vector"
          , "statistics"
          , "aeson"
          , "newtype"
          , "scientific"
          , "uom-plugin"
          , "detour-via-sci"
          , "detour-via-uom"
          , "siggy-chardust"
          , "flight-units"
          , "tasty"
          , "tasty-hunit"
          , "tasty-quickcheck"
          , "tasty-smallcheck"
          , "smallcheck"
          , "QuickCheck"
          , "quickcheck-instances"
          ]

in  let testopts =
          [ "-rtsopts"
          , "-threaded"
          , "-with-rtsopts=-N"
          , "-fplugin Data.UnitsOfMeasure.Plugin"
          ]

in    defs
    ⫽ ./../default-extensions.dhall
    ⫽ { flags =
          { suppress-failing-tests = { manual = False, default = True } }
      , name =
          "flight-gap-math"
      , synopsis =
          "GAP Scoring, Math"
      , description =
          "GAP scoring for hang gliding and paragliding competitons, the math of how to add points and apply penalties."
      , category =
          "Flight"
      , github =
          "blockscope/flare-timing/lang-haskell/gap-lead"
      , ghc-options =
          [ "-Wall"
          , "-fplugin Data.UnitsOfMeasure.Plugin"
          , "-fno-warn-partial-type-signatures"
          ]
      , dependencies =
            defs.dependencies
          # [ "aeson"
            , "cassava"
            , "containers"
            , "facts"
            , "newtype"
            , "numbers"
            , "QuickCheck"
            , "scientific"
            , "template-haskell"
            , "text"
            , "uom-plugin"
            , "detour-via-sci"
            , "detour-via-uom"
            , "siggy-chardust"
            , "flight-units"
            , "flight-gap-base"
            , "flight-gap-weight"
            , "flight-gap-valid"
            ]
      , library =
          { source-dirs = "library", exposed-modules = [ "Flight.Score" ] }
      , tests =
            ./../default-tests.dhall
          ⫽ { score =
                { dependencies =
                    testdeps # [ "facts", "flight-gap-math" ]
                , ghc-options =
                    testopts
                , main =
                    "ScoreTestMain.hs"
                , source-dirs =
                    [ "test-suite/test", "test-suite/score" ]
                }
            , stop =
                { dependencies =
                    testdeps
                , ghc-options =
                    testopts
                , main =
                    "StopTestMain.hs"
                , source-dirs =
                    [ "library", "test-suite/test", "test-suite/stop" ]
                }
            , doctest =
                { dependencies =
                      defs.dependencies
                    # [ "quickcheck-classes"
                      , "numbers"
                      , "doctest"
                      , "facts"
                      , "flight-units"
                      ]
                , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                , main =
                    "DocTest.hs"
                , source-dirs =
                    [ "library", "test-suite-doctest" ]
                , when =
                    { condition =
                        "flag(suppress-failing-tests)"
                    , buildable =
                        False
                    }
                }
            }
      }
