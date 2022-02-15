let mkHome = ./../home.dhall

in  let defs = ./../defaults.dhall
    
    in    defs
        ⫽ { name =
              "tasty-compare"
          , homepage =
              mkHome "lang-haskell/tasty-compare#readme"
          , synopsis =
              "Tasty HUnit extensions for comparisons."
          , description =
              "Adds assertCompare and operators for the same."
          , category =
              "Test"
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
