let defs = ./../defaults.dhall

in  let deps =
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
          , "sampling"
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
          , "flight-gap-allot"
          , "flight-gap-effort"
          , "flight-gap-lead"
          , "flight-gap-math"
          , "flight-gap-stop"
          , "flight-gap-valid"
          , "flight-gap-weight"
          , "flight-kml"
          , "flight-latlng"
          , "flight-mask"
          , "flight-route"
          , "flight-scribe"
          , "flight-units"
          , "flight-zone"
          , "detour-via-sci"
          , "siggy-chardust"
          ]

    in    defs
        ⫽ ./../default-extensions.dhall
        ⫽ { name = "app-serve"
          , homepage =
              "https://github.com/blockscope/flare-timing/lang-haskell/app-serve#readme"
          , synopsis =
              "A collection of apps and libraries for scoring hang gliding and paragliding competitions."
          , description =
              "Scoring and viewing hang gliding and paragliding competitions."
          , category = "Data, Parsing"
          , dependencies = defs.dependencies
          , ghc-options = [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
          , executables =
            { ft-comp-py =
              { dependencies = deps # [ "servant-py" ]
              , other-modules =
                [ "Serve.Alt"
                , "Serve.Api"
                , "Serve.App"
                , "Serve.Area"
                , "Serve.Config"
                , "Serve.Error"
                , "Serve.Pilot"
                , "Serve.PointDiff"
                , "Serve.Route"
                , "Serve.Task"
                , "Serve.Track"
                , "Serve.Validity"
                , "ServeOptions"
                , "ServeSwagger"
                ]
              , main = "GenPyClient.hs"
              , source-dirs = "src"
              }
            , ft-comp-serve =
              { dependencies = deps
              , ghc-options = [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , other-modules =
                [ "Serve.Alt"
                , "Serve.Api"
                , "Serve.App"
                , "Serve.Area"
                , "Serve.Config"
                , "Serve.Error"
                , "Serve.Pilot"
                , "Serve.PointDiff"
                , "Serve.Route"
                , "Serve.Task"
                , "Serve.Track"
                , "Serve.Validity"
                , "ServeOptions"
                , "ServeSwagger"
                ]
              , main = "ServeMain.hs"
              , source-dirs = "src"
              }
            }
          }
