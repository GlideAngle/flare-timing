    let deps = ./../defaults.dhall

in    deps
    ⫽ ./../default-extensions.dhall
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
            , "flight-clip"
            , "flight-comp"
            , "flight-latlng"
            , "flight-gap"
            , "flight-kml"
            , "flight-mask"
            , "flight-route"
            , "flight-zone"
            , "detour-via-sci"
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
          ./../default-tests.dhall
      }
