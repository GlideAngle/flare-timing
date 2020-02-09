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
      , ghc-options =
          [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
      , executables =
          { comp-serve =
              { dependencies =
                  [ "aeson"
                  , "bytestring"
                  , "cmdargs"
                  , "containers"
                  , "directory"
                  , "filepath"
                  , "filemanip"
                  , "lens"
                  , "mtl"
                  , "raw-strings-qq"
                  , "safe-exceptions"
                  , "servant"
                  , "servant-server"
                  , "servant-swagger"
                  , "servant-swagger-ui"
                  , "swagger2"
                  , "statistics"
                  , "text"
                  , "time"
                  , "transformers"
                  , "vector"
                  , "wai"
                  , "wai-cors"
                  , "wai-extra"
                  , "warp"
                  , "yaml"
                  , "uom-plugin"
                  , "flight-cmd"
                  , "flight-clip"
                  , "flight-earth"
                  , "flight-comp"
                  , "flight-gap"
                  , "flight-kml"
                  , "flight-latlng"
                  , "flight-mask"
                  , "flight-route"
                  , "flight-scribe"
                  , "flight-zone"
                  , "siggy-chardust"
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
