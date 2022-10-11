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
                  , name = "flight-gap-math"
                  , homepage = mkHome "lang-haskell/gap-math#readme"
                  , synopsis = "GAP Scoring, Math"
                  , description =
                      "GAP scoring for hang gliding and paragliding competitons, the math of how to add points and apply penalties."
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
                        , "QuickCheck >= 2.14.2"
                        , "scientific"
                        , "template-haskell"
                        , "text"
                        , "uom-plugin"
                        , "detour-via-sci"
                        , "detour-via-uom"
                        , "siggy-chardust"
                        , "flight-units"
                        , "flight-gap-allot"
                        , "flight-gap-weight"
                        , "flight-gap-valid"
                        ]
                  , library =
                    { source-dirs = "library"
                    , exposed-modules = "Flight.Score"
                    }
                  , tests =
                        ./../default-tests.dhall
                      ⫽ { math =
                          { dependencies =
                              testdeps # [ "facts", "flight-gap-math" ]
                          , ghc-options = testopts
                          , main = "MathTestMain.hs"
                          , source-dirs = "test-suite-math"
                          }
                        , doctest =
                          { dependencies =
                                defs.dependencies
                              # [ "quickcheck-classes"
                                , "exact-real"
                                , "doctest"
                                , "facts"
                                , "flight-units"
                                ]
                          , ghc-options =
                            [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                          , main = "DocTest.hs"
                          , source-dirs = "test-suite-doctest"
                          }
                        }
                  }
