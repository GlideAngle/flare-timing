  ./defaults.dhall 
⫽ { name =
      "siggy-chardust"
  , synopsis =
      "Rounding keeping decimal places and significant digits."
  , description =
      "Round to a certain number of decimal places or significant digits."
  , category =
      "Data, Math"
  , github =
      "blockscope/flare-timing/siggy-chardust"
  , library =
      { dependencies =
          "base >=4.5 && <5"
      , source-dirs =
          "library"
      , exposed-modules =
          "Data.Number.RoundingFunctions"
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
