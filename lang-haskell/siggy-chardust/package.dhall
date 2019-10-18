    let defs = ./../defaults.dhall

in    defs
    ⫽ { name =
          "siggy-chardust"
      , version =
          "1.0.0"
      , synopsis =
          "Rounding rationals to significant digits and decimal places."
      , description =
          "The round function from the prelude returns an integer. The standard librarys of C and C++ have round functions that return floating point numbers. Rounding in this library takes and returns rationals and can round to a number of significant digits or a number of decimal places."
      , category =
          "Data, Math, Numeric"
      , github =
          "blockscope/flare-timing/siggy-chardust"
      , homepage =
          "https://github.com/blockscope/flare-timing/tree/master/siggy-chardust#readme"
      , library =
          { source-dirs = "library", exposed-modules = "Data.Ratio.Rounding" }
      , tests =
            ./../default-tests.dhall
          ⫽ { doctest =
                { dependencies =
                    [ "base", "doctest" ]
                , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                , main =
                    "DocTest.hs"
                , source-dirs =
                    [ "library", "test-suite-doctest" ]
                }
            , digits =
                { dependencies =
                    [ "tasty"
                    , "tasty-hunit"
                    , "tasty-quickcheck"
                    , "tasty-smallcheck"
                    , "smallcheck"
                    ]
                , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                , main =
                    "Rounding.hs"
                , source-dirs =
                    [ "library", "test-suite-digits" ]
                }
            }
      }
