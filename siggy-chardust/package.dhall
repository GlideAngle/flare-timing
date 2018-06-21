  ./defaults.dhall 
⫽ { name =
      "siggy-chardust"
  , synopsis =
      "Rounding rationals to significant digits and decimal places."
  , description =
      "The round function from the prelude returns an integer. The standard librarys of C and C++ have round functions that return floating point numbers. Rounding in this library takes and returns rationals and can round to a number of significant digits or a number of decimal places."
  , category =
      "Data, Math"
  , github =
      "blockscope/flare-timing/siggy-chardust"
  , library =
      { dependencies =
          "base >=4.8 && <5"
      , source-dirs =
          "library"
      , exposed-modules =
          "Data.Ratio.Rounding"
      }
  , tests =
      { hlint =
          { dependencies =
              [ "base", "hlint" ]
          , ghc-options =
              [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
          , main =
              "HLint.hs"
          , source-dirs =
              [ "library", "test-suite-hlint" ]
          }
      , doctest =
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
              [ "base"
              , "tasty"
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
