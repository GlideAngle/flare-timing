    let defs = ./../defaults.dhall

in    defs
    ⫽ ./../default-extensions.dhall
    ⫽ { name =
          "app-serve"
      , synopsis =
          "A collection of apps and libraries for scoring hang gliding and paragliding competitions."
      , description =
          "Scoring and viewing hang gliding and paragliding competitions."
      , category =
          "Data, Parsing"
      , github =
          "blockscope/flare-timing/app-serve"
      , dependencies =
          defs.dependencies
      , executables =
          { comp-serve =
              { dependencies =
                  [ "servant"
                  , "servant-server"
                  , "transformers"
                  , "aeson"
                  , "wai"
                  , "wai-cors"
                  , "warp"
                  , "directory"
                  , "filepath"
                  , "filemanip"
                  , "raw-strings-qq"
                  , "cmdargs"
                  , "mtl"
                  , "yaml"
                  , "bytestring"
                  , "flight-cmd"
                  , "flight-comp"
                  ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "ServeMain.hs"
              , source-dirs =
                  "comp-serve"
              }
          }
      , tests =
          { hlint =
              { dependencies =
                  [ "base", "hlint", "flight-comp" ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "HLint.hs"
              , source-dirs =
                  "test-suite-hlint"
              }
          }
      }
