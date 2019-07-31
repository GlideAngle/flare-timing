    let defs = ./../defaults.dhall

in  let exts = ./../default-extensions.dhall

in    defs
    ⫽ { name =
          "flight-earth"
      , synopsis =
          "Distances on the WGS84 ellipsoid, the FAI sphere and the UTM projection."
      , description =
          "Distances on the Earth for hang gliding and paragliding competitons."
      , category =
          "Flight"
      , github =
          "blockscope/flare-timing/earth"
      , ghc-options =
          [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
      , default-extensions =
            exts.default-extensions
          # [ "AllowAmbiguousTypes", "InstanceSigs", "UndecidableSuperClasses" ]
      , dependencies =
            defs.dependencies
          # [ "aeson"
            , "numbers"
            , "fgl"
            , "uom-plugin"
            , "bifunctors"
            , "aeson"
            , "scientific"
            , "mtl"
            , "text"
            , "hcoord"
            , "hcoord-utm"
            , "detour-via-sci"
            , "detour-via-uom"
            , "siggy-chardust"
            , "flight-latlng"
            , "flight-units"
            , "flight-zone"
            ]
      , library =
          { source-dirs =
              "library"
          , exposed-modules =
              [ "Flight.Earth.Ellipsoid"
              , "Flight.Earth.Flat"
              , "Flight.Earth.Flat.Double"
              , "Flight.Earth.Flat.Rational"
              , "Flight.Earth.Sphere"
              , "Flight.Geodesy"
              , "Flight.Geodesy.Solution"
              , "Flight.Geodesy.Double"
              , "Flight.Geodesy.Rational"
              ]
          }
      , tests =
            ./../default-tests.dhall
          ⫽ { earth =
                { dependencies =
                    [ "tasty"
                    , "tasty-hunit"
                    , "tasty-quickcheck"
                    , "tasty-smallcheck"
                    , "smallcheck"
                    , "tasty-compare"
                    ]
                , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                , main =
                    "Earth.hs"
                , source-dirs =
                    [ "library", "test-suite-earth" ]
                }
            , doctest =
                { dependencies =
                    [ "doctest" ]
                , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                , main =
                    "DocTest.hs"
                , source-dirs =
                    [ "library", "test-suite-doctest" ]
                }
            }
      }
