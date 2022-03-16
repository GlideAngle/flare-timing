let mkHome = ./../home.dhall

in  let deps = ./../defaults.dhall

    in    deps
        ⫽ ./../default-extensions.dhall
        ⫽ { name = "flight-lookup"
          , homepage = mkHome "lang-haskell/lookup#readme"
          , synopsis = "Hang gliding and paragliding competition data access."
          , description = "Lookup items for a task, for a pilot, etc."
          , category = "Data"
          , ghc-options = [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
          , dependencies =
                deps.dependencies
              # [ "aeson"
                , "bytestring"
                , "cassava"
                , "containers"
                , "directory"
                , "filemanip"
                , "filepath"
                , "lens"
                , "mtl"
                , "path"
                , "scientific"
                , "split"
                , "time"
                , "unordered-containers"
                , "uom-plugin"
                , "flight-clip"
                , "flight-comp"
                , "flight-gap-allot"
                , "flight-gap-math"
                , "flight-kml"
                , "flight-latlng"
                , "flight-mask"
                , "flight-route"
                , "flight-zone"
                , "detour-via-sci"
                ]
          , library =
            { source-dirs = "library"
            , exposed-modules =
              [ "Flight.Lookup.Route"
              , "Flight.Lookup.Cross"
              , "Flight.Lookup.Tag"
              , "Flight.Lookup.Stop"
              , "Flight.Lookup"
              ]
            }
          , tests = ./../default-tests.dhall
          }
