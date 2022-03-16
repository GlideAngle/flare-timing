-- SEE: https://github.com/sol/hpack/issues/254
let mkHome = ./../home.dhall

in  let defs = ./../defaults.dhall

    in    defs
        ⫽ ./../default-extensions.dhall
        ⫽ { name = "flight-zone"
          , homepage = mkHome "lang-haskell/zone#readme"
          , synopsis = "Control zones to fly."
          , description =
              "Control zones for hang gliding and paragliding competitons."
          , category = "Flight"
          , ghc-options = [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
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
            { source-dirs = "library"
            , exposed-modules =
              [ "Flight.Zone"
              , "Flight.Zone.ZoneKind"
              , "Flight.Zone.MkZones"
              , "Flight.Zone.TaskZones"
              , "Flight.Zone.Path"
              , "Flight.Zone.Raw"
              , "Flight.Zone.SpeedSection"
              , "Flight.Zone.Cylinder"
              ]
            }
          , tests =
                ./../default-tests.dhall
              ⫽ { serial =
                  { dependencies =
                    [ "text"
                    , "yaml"
                    , "here"
                    , "bytestring"
                    , "aeson-pretty"
                    , "tasty"
                    , "tasty-hspec"
                    , "tasty-discover"
                    , "tasty-golden"
                    ]
                  , verbatim.build-tool-depends
                    = "tasty-discover:tasty-discover"
                  , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                  , main = "Spec.hs"
                  , source-dirs = [ "library", "test-suite-serial" ]
                  }
                , doctest =
                  { dependencies = [ "doctest", "QuickCheck" ]
                  , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                  , main = "DocTest.hs"
                  , source-dirs = [ "library", "test-suite-doctest" ]
                  }
                }
          }
