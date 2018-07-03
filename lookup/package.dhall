    let deps = ./defaults.dhall 

in    deps
    ⫽ ./default-extensions.dhall 
    ⫽ { name =
          "flight-lookup"
      , synopsis =
          "Hang gliding and paragliding competition data access."
      , description =
          "Lookup items for a task, for a pilot, etc."
      , category =
          "Data"
      , github =
          "blockscope/flare-timing/lookup"
      , ghc-options =
          [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
      , dependencies =
            deps.dependencies
          # [ "split"
            , "path"
            , "aeson"
            , "scientific"
            , "containers"
            , "unordered-containers"
            , "time"
            , "cassava"
            , "bytestring"
            , "directory"
            , "filepath"
            , "filemanip"
            , "lens"
            , "mtl"
            , "uom-plugin"
            , "detour-via-sci"
            , "flight-latlng"
            , "flight-zone"
            , "flight-route"
            , "flight-gap"
            , "flight-comp"
            , "flight-kml"
            , "flight-mask"
            ]
      , library =
          { source-dirs =
              "library"
          , exposed-modules =
              [ "Flight.Lookup.Route"
              , "Flight.Lookup.Cross"
              , "Flight.Lookup.Tag"
              , "Flight.Lookup"
              ]
          }
      , tests =
          { hlint =
              { dependencies =
                  [ "hlint" ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "HLint.hs"
              , source-dirs =
                  [ "library", "test-suite-hlint" ]
              }
          }
      }
