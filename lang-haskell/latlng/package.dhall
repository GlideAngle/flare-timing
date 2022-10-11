let mkHome = ./../home.dhall

in  let deps = ./../defaults.dhall

    in    deps
        ⫽ ./../default-extensions.dhall
        ⫽ { name = "flight-latlng"
          , homepage = mkHome "lang-haskell/latlng#readme"
          , synopsis =
              "Latitude and longitude as used in hang gliding and paragliding competitions."
          , description = "Latitude and longitude."
          , category = "Flight"
          , ghc-options = [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
          , dependencies =
                deps.dependencies
              # [ "numbers"
                , "random"
                , "aeson"
                , "bytestring"
                , "bifunctors"
                , "cassava"
                , "deepseq"
                , "uom-plugin"
                , "formatting"
                , "text"
                , "newtype"
                , "tasty-quickcheck"
                , "smallcheck"
                , "detour-via-sci"
                , "detour-via-uom"
                , "siggy-chardust"
                , "flight-units"
                ]
          , library =
            { source-dirs = "library"
            , exposed-modules =
              [ "Flight.Field"
              , "Flight.EastNorth"
              , "Flight.LatLng"
              , "Flight.LatLng.Raw"
              , "Flight.LatLng.RawLatLng"
              , "Flight.LatLng.Double"
              , "Flight.LatLng.Float"
              , "Flight.LatLng.Rational"
              , "Flight.Distance"
              ]
            }
          , tests = ./../default-tests.dhall
          }
