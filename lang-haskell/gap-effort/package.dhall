let mkHome = ./../home.dhall

in  let defs = ./../defaults.dhall

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
                ⫽ { flags.suppress-failing-tests
                    =
                    { manual = False, default = True }
                  , name = "flight-gap-effort"
                  , homepage = mkHome "lang-haskell/gap-effort#readme"
                  , synopsis = "GAP Scoring, Effort"
                  , description =
                      "GAP scoring for hang gliding and paragliding competitons, the effort (distance difficulty) parts."
                  , category = "Flight"
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
                        , "exact-real"
                        , "QuickCheck"
                        , "scientific"
                        , "template-haskell"
                        , "text"
                        , "uom-plugin"
                        , "detour-via-sci"
                        , "detour-via-uom"
                        , "siggy-chardust"
                        , "flight-units"
                        , "flight-gap-allot"
                        ]
                  , library =
                    { source-dirs = "library"
                    , exposed-modules = "Flight.Score"
                    }
                  , tests =
                        ./../default-tests.dhall
                      ⫽ { effort =
                          { dependencies = testdeps # [ "flight-gap-effort" ]
                          , ghc-options = testopts
                          , main = "EffortTestMain.hs"
                          , source-dirs = "test-suite-effort"
                          , when =
                            { condition = "flag(suppress-failing-tests)"
                            , buildable = False
                            }
                          }
                        , doctest =
                          { dependencies =
                                defs.dependencies
                              # [ "quickcheck-classes"
                                , "exact-real"
                                , "doctest"
                                , "facts"
                                , "flight-units"
                                , "flight-gap-allot"
                                ]
                          , ghc-options =
                            [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                          , main = "DocTest.hs"
                          , source-dirs = "test-suite-doctest"
                          , when =
                            { condition = "flag(suppress-failing-tests)"
                            , buildable = False
                            }
                          }
                        }
                  }
