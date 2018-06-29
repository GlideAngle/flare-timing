  ./defaults.dhall 
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
  , library =
      { dependencies =
          [ "base >=4.5 && <5"
          , "numbers"
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
          , "siggy-chardust"
          , "flight-units"
          ]
      , source-dirs =
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
      { hlint =
          { dependencies =
              [ "base"
              , "numbers"
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
              , "siggy-chardust"
              , "flight-units"
              , "hlint"
              ]
          , ghc-options =
              [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
          , main =
              "HLint.hs"
          , source-dirs =
              [ "library", "test-suite-hlint" ]
          }
      }
  }
