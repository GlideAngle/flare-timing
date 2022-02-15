let mkHome = ./../home.dhall

in  let defs =
          ./../defaults.dhall
    
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
                      { suppress-failing-tests =
                          { manual = False, default = True }
                      }
                  , name =
                      "flight-gap-allot"
                  , homepage =
                      mkHome "lang-haskell/gap-allot#readme"
                  , synopsis =
                      "GAP Scoring, allotment."
                  , description =
                      "GAP scoring for hang gliding and paragliding competitons, allot points available for each part."
                  , category =
                      "Flight"
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
                        , "deepseq"
                        , "exact-real"
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
                          "Flight.Score"
                      }
                  , tests =
                        ./../default-tests.dhall
                      ⫽ { doctest =
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
                                "test-suite-doctest"
                            }
                        }
                  }
