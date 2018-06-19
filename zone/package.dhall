  ./defaults.dhall 
⫽ { name =
      "flight-zone"
  , synopsis =
      "Control zones to fly."
  , description =
      "Control zones for hang gliding and paragliding competitons."
  , category =
      "Flight"
  , github =
      "blockscope/flare-timing/zone"
  , ghc-options =
      [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
  , library =
      { dependencies =
          [ "base >=4.5 && <5"
          , "uom-plugin"
          , "aeson"
          , "scientific"
          , "newtype"
          , "siggy-chardust"
          , "detour-via-sci"
          , "detour-via-uom"
          , "flight-units"
          , "flight-latlng"
          ]
      , source-dirs =
          "library"
      , exposed-modules =
          [ "Flight.Zone"
          , "Flight.Zone.Path"
          , "Flight.Zone.Raw"
          , "Flight.Zone.Cylinder"
          ]
      }
  , tests =
      { hlint =
          { dependencies =
              [ "base"
              , "hlint"
              , "uom-plugin"
              , "aeson"
              , "scientific"
              , "newtype"
              , "siggy-chardust"
              , "detour-via-sci"
              , "detour-via-uom"
              , "flight-units"
              , "flight-latlng"
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
