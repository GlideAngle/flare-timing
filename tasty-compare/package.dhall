  ./defaults.dhall 
⫽ { name =
      "tasty-compare"
  , synopsis =
      "Tasty HUnit extensions for comparisons."
  , description =
      "Adds assertCompare and operators for the same."
  , category =
      "Test"
  , github =
      "blockscope/flare-timing/tasty-compare"
  , library =
      { dependencies =
          [ "base", "tasty", "tasty-hunit", "call-stack" ]
      , source-dirs =
          "library"
      , exposed-modules =
          "Test.Tasty.HUnit.Compare"
      }
  , tests =
      { hlint =
          { dependencies =
              [ "base", "hlint", "tasty", "tasty-hunit", "call-stack" ]
          , ghc-options =
              [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
          , main =
              "HLint.hs"
          , source-dirs =
              [ "library", "test-suite-hlint" ]
          }
      }
  }
