    let defs = ./defaults.dhall 

in    defs
    ⫽ ./default-extensions.dhall 
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
      , dependencies =
            defs.dependencies
          # [ "uom-plugin"
            , "aeson"
            , "scientific"
            , "newtype"
            , "siggy-chardust"
            , "detour-via-sci"
            , "detour-via-uom"
            , "flight-units"
            , "flight-latlng"
            ]
      , library =
          { source-dirs =
              "library"
          , exposed-modules =
              [ "Flight.Zone"
              , "Flight.Zone.ZoneKind"
              , "Flight.Zone.MkZones"
              , "Flight.Zone.TaskZones"
              , "Flight.Zone.Path"
              , "Flight.Zone.Raw"
              , "Flight.Zone.Cylinder"
              ]
          }
      , tests =
            ./default-tests.dhall 
          ⫽ { json =
                { dependencies =
                    [ "text"
                    , "yaml"
                    , "here"
                    , "bytestring"
                    , "aeson-pretty"
                    , "tasty"
                    , "tasty-hspec"
                    , "tasty-discover"
                    ]
                , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                , main =
                    "Spec.hs"
                , source-dirs =
                    [ "library", "test-suite-json" ]
                }
            }
      }
