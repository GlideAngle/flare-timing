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
          "flight-gap"
      , synopsis =
          "GAP Scoring."
      , description =
          "GAP scoring for hang gliding and paragliding competitons."
      , category =
          "Flight"
      , github =
          "blockscope/flare-timing/gap"
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
            ]
      , library =
          { source-dirs =
              "library"
          , exposed-modules =
              [ "Flight.Score", "Flight.Gap.Fraction" ]
          }
      , tests =
            ./../default-tests.dhall
          ⫽ { effort =
                { dependencies =
                    testdeps
                , ghc-options =
                    testopts
                , main =
                    "EffortTestMain.hs"
                , source-dirs =
                    [ "library", "test-suite/test", "test-suite/effort" ]
                , when =
                    { condition =
                        "flag(suppress-failing-tests)"
                    , buildable =
                        False
                    }
                }
            , leading =
                { dependencies =
                    testdeps
                , ghc-options =
                    testopts
                , main =
                    "LeadingTestMain.hs"
                , source-dirs =
                    [ "library", "test-suite/test", "test-suite/leading" ]
                }
            , score =
                { dependencies =
                    testdeps # [ "facts" ]
                , ghc-options =
                    testopts
                , main =
                    "ScoreTestMain.hs"
                , source-dirs =
                    [ "library", "test-suite/test", "test-suite/score" ]
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
            , validity =
                { dependencies =
                    testdeps
                , ghc-options =
                    testopts
                , main =
                    "ValidityTestMain.hs"
                , source-dirs =
                    [ "library", "test-suite/test", "test-suite/validity" ]
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
