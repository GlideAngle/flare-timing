    let deps = ./defaults.dhall 

in    deps
    ⫽ ./default-extensions.dhall 
    ⫽ { name =
          "flight-latlng"
      , synopsis =
          "Latitude and longitude as used in hang gliding and paragliding competitions."
      , description =
          "Latitude and longitude."
      , category =
          "Flight"
      , github =
          "blockscope/flare-timing/latlng"
      , ghc-options =
          [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
      , dependencies =
            deps.dependencies
          # [ "numbers"
            , "random"
            , "aeson"
            , "cassava"
            , "bytestring"
            , "bifunctors"
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
          { source-dirs =
              "library"
          , exposed-modules =
              [ "Flight.Field"
              , "Flight.EastNorth"
              , "Flight.LatLng"
              , "Flight.LatLng.Raw"
              , "Flight.LatLng.Double"
              , "Flight.LatLng.Float"
              , "Flight.LatLng.Rational"
              , "Flight.Distance"
              ]
          }
      , tests =
          ./default-tests.dhall 
      }
