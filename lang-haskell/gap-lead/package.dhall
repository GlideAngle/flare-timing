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
                      "flight-gap-lead"
                  , homepage =
                      mkHome "lang-haskell/gap-lead#readme"
                  , synopsis =
                      "GAP Scoring, Leading"
                  , description =
                      "GAP scoring for hang gliding and paragliding competitons, the leading parts."
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
                        , "flight-gap-allot"
                        ]
                  , library =
                      { source-dirs =
                          "library"
                      , exposed-modules =
                          "Flight.Score"
                      }
                  , tests =
                        ./../default-tests.dhall
                      ⫽ { lead =
                            { dependencies =
                                testdeps # [ "flight-gap-lead" ]
                            , ghc-options =
                                testopts
                            , main =
                                "LeadTestMain.hs"
                            , source-dirs =
                                "test-suite-lead"
                            }
                        }
                  }
