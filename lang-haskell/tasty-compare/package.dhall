    let defs = ./../defaults.dhall

in    defs
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
      , dependencies =
          defs.dependencies # [ "tasty", "tasty-hunit", "call-stack" ]
      , library =
          { source-dirs =
              "library"
          , exposed-modules =
              "Test.Tasty.HUnit.Compare"
          }
      , tests =
          ./../default-tests.dhall
      }
